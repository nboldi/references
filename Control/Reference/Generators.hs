{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- | Functions to create references from simple functions 
-- and members of the lens library.
module Control.Reference.Generators where

import Control.Reference.Representation
import Control.Reference.Types

import Control.Instances.Morph
import qualified Data.Traversable as Trav
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer

-- | Generates a traversal for any 'Trav.Traversable' 'Functor'
traverse :: (Trav.Traversable t) => Traversal (t a) (t b) a b
traverse = reference (morph . execWriter . Trav.mapM (tell . (:[])))
                     (Trav.mapM . const . return) 
                     Trav.mapM
             
-- | Generate a lens from a pair of inverse functions
iso :: (a -> b) -> (b -> a) -> Simple Iso a b
iso f g = bireference (return . f) (\b _ -> return . g $ b) (\trf a -> trf (f a) >>= return . g  ) 
                      (return . g) (\a _ -> return . f $ a) (\trf b -> trf (g b) >>= return . f  ) 
             
iso' :: (a -> b) -> (a' -> b') -> (b -> a) -> (b' -> a') -> Iso a a' b b'
iso' f f' g g' 
  = bireference (return . f) (\b _ -> return . g' $ b) (\trf a -> trf (f a) >>= return . g'  ) 
                (return . g) (\a _ -> return . f' $ a) (\trf b -> trf (g b) >>= return . f'  ) 

-- | Generates a lens from a getter and a setter
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens get set = reference (return . get) 
                         (\b -> return . set b ) 
                         (\f a -> f (get a) >>= \b -> return $ set b a)

-- | Creates a polymorphic partial lense
--
-- @Either t a@ is used instead of @Maybe a@ to permit the types of 's' and 't' to differ.
partial :: (s -> Either t (a, b -> t)) -> Partial s t a b
partial access 
  = reference 
      (either (const $ morph Nothing) (return . fst) . access)
      (\b -> return . either id (($b) . snd) . access)
      (\f -> either return (\(a,set) -> f a >>= return . set) . access)
         
-- | Creates a polymorphic partial lens that can be turned to give a total lens
prism :: (a -> s) -> (b -> t) -> (s -> Either t a) -> (t -> Maybe b) -> Prism s t a b
prism back back' access access'
  = bireference (either (const $ morph Nothing) return . access)
                (\b -> return . either id (const $ (back' b)) . access)
                (\f -> either return (f >=> return . back') . access)
                (return . back)
                (\t _ -> morph $ access' t)
                (\f a -> f (back a) >>= morph . access')
                
-- | Creates a monomorphic partial lens that can be turned to give a total lens
simplePrism :: (a -> s) -> (s -> Maybe a) -> Prism s s a a
simplePrism back access = prism back back (\s -> maybe (Left s) Right (access s)) access
                
-- | Creates a simple partial lens
simplePartial :: (s -> Maybe (a, a -> s)) -> Partial s s a a
simplePartial access 
  = partial (\s -> maybe (Left s) Right (access s))

                                                     
-- | Clones a lens from "Control.Lens"
fromLens :: (forall f . Functor f => (a -> f b) -> s -> f t) -> Lens s t a b
fromLens l = reference (\s -> return (getConst $ l Const s))
                       (\b -> return . (runIdentity . l (\_ -> Identity b)))
                       l
                 
-- | Clones a traversal from "Control.Lens"
fromTraversal :: (forall f . Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
fromTraversal l = reference (morph . execWriter . l (\a -> tell [a] >> return undefined))
                            (\b -> return . (runIdentity . l (\_ -> Identity b)))
                            l

-- | Filters the traversed elements with a given predicate. 
-- Has specific versions for traversals and partial lenses.
filtered :: (a -> Bool) -> Simple RefPlus a a
filtered p = reference (\s -> if p s then return s else mzero)
                       (\a s -> if p s then return a else return s)
                       (\f s -> if p s then f s else return s)