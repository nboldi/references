{-# LANGUAGE LambdaCase, TupleSections, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}

-- | Predefined references for commonly used data structures.
--
-- When defining lenses one should use the more general types. For instance 'Lens' instead of the more strict 'Lens''. This way references with different @m1@ and @m2@ monads can be combined if there is a monad @m'@ for @m1 !<! m'@ and @m2 !<! m'@.

-- TODO: create references that can add or remove elements with prisms
module Control.Reference.Predefined where

import Control.Reference.Representation

import Control.Concurrent
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Data.Either.Combinators
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import qualified Control.Lens as Lens
import qualified Data.Traversable as Trav

-- * Trivial references

-- | An identical lens. Accesses the context.
--
-- > self & a = a & self = a
self :: Lens a b a b
self = reference return (const . return) id   

-- | An empty reference that do not traverse anything
--
-- > emptyRef &+& a = a &+& emptyRef = a
emptyRef :: Simple RefPlus s a
emptyRef = reference (const mzero) (const return) (const return)


-- * Reference generators

-- | Generates a traversal for any 'Trav.Traversable' 'Functor'
traverse :: (Trav.Traversable t) => Traversal (t a) (t b) a b
traverse = reference (liftMS . execWriter . Trav.mapM (tell . (:[])))
                     (Trav.mapM . const . return) 
                     Trav.mapM
             
-- | Generate a lens from a pair of inverse functions
iso :: (a -> b) -> (b -> a) -> Lens a a b b
iso f g = reference (return . f) (\b _ -> return . g $ b) (\trf a -> trf (f a) >>= return . g  ) 

-- | Generates a lens from a getter and a setter
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens get set = reference (return . get) 
                         (\b -> return . set b ) 
                         (\f a -> f (get a) >>= \b -> return $ set b a)

-- | Creates a monomorphic partial lense
partial :: (s -> Either t (a, b -> t)) -> LensPart s t a b
partial access = reference (\s -> case access s of Left _ -> liftMS Nothing
                                                   Right (a,_) -> return a)
                           (\b s -> case access s of Left t -> return t
                                                     Right (_,set) -> return (set b))
                           (\f s -> case access s of Left t -> return t
                                                     Right (a,set) -> f a >>= return . set)

-- | Clones a lens from "Control.Lens"
fromLens :: Lens.Lens s s a a -> Lens.Lens s t a b -> Lens s t a b
fromLens lm lp = reference (\s -> return (s Lens.^. lm)) 
                           (\b -> return . (lp Lens..~ b))
                           lp              
                           
-- | Clones a traversal from "Control.Lens"
fromTraversal :: Lens.Traversal s t a b -> Traversal s t a b
fromTraversal l = reference (liftMS . execWriter . Lens.mapMOf l (\a -> tell [a] >> return undefined))
                            (\b -> return . Lens.set l b) l
                                                           
-- | Filters the traversed elements with a given predicate. 
-- Has specific versions for traversals and partial lenses.
filtered :: (a -> Bool) -> Simple RefPlus a a
filtered p = reference (\s -> if p s then return s else mzero)
                       (\a s -> if p s then return a else return s)
                       (\f s -> if p s then f s else return s)

-- * References for simple data structures

-- | A partial lens to access the value that may not exist
just :: LensPart (Maybe a) (Maybe b) a b
just = partial (\case Just x -> Right (x, Just)
                      Nothing -> Left Nothing)

-- | A partial lens to access the right option of an 'Either'
right :: LensPart (Either a b) (Either a c) b c
right = partial (\case Right x -> Right (x, Right)
                       Left a -> Left (Left a))

-- | A partial lens to access the left option of an 'Either'                  
left :: LensPart (Either a c) (Either b c) a b
left = partial (\case Left a -> Right (a, Left)
                      Right r -> Left (Right r))

-- | Access the value that is in the left or right state of an 'Either'
anyway :: Lens (Either a a) (Either b b) a b
anyway = reference (either return return)
                   (\b -> return . mapBoth (const b) (const b))
                   (\f -> either (f >=> return . Left) (f >=> return . Right))

-- | References both elements of a tuple
both :: Traversal (a,a) (b,b) a b
both = reference (\(x,y) -> liftMS [x,y]) 
                 (\v -> return . const (v,v)) 
                 (\f (x,y) -> (,) <$> f x <*> f y)

-- | References the head of a list
_head :: Simple LensPart [a] a
_head = partial (\case [] -> Left []; x:xs -> Right (x,(:xs)))
    
-- | References the tail of a list
_tail :: Simple LensPart [a] [a]
_tail = partial (\case [] -> Left []; x:xs -> Right (xs,(x:)))
                 
-- | Lenses for given values in a data structure that is indexed by keys.
class Association e where
  type AssocIndex e :: *
  type AssocElem e :: *
  element :: AssocIndex e -> Simple LensPart e (AssocElem e)
          
instance Association [a] where          
  type AssocIndex [a] = Int
  type AssocElem [a] = a
  element i = reference (liftMS . at i) (\v -> upd (const (return v)))
                        upd
    where at :: Int -> [a] -> Maybe a
          at n _ | n < 0  = Nothing
          at _ []         = Nothing
          at 0 (x:_)      = Just x
          at n (_:xs)     = at (n-1) xs
          
          upd :: Monad w => (a -> w a) -> [a] -> w [a]
          upd f ls = let (before,rest) = splitAt i ls
                      in case rest of [] -> return before
                                      (x:xs) -> f x >>= \fx -> return $ before ++ fx : xs
  
instance Ord k => Association (Map k v) where
  type AssocIndex (Map k v) = k
  type AssocElem (Map k v) = v
  element k = reference (liftMS . Map.lookup k)
                        (\v -> return . insert k v) 
                        (\trf m -> case Map.lookup k m of Just x -> trf x >>= \x' -> return (insert k x' m)
                                                          Nothing -> return m)

-- * Stateful references
                                                          
-- | Access a value inside an MVar. Writing should only be used for initial 
-- assignment or parts of the program will block infinitely. Reads and updates are done in sequence,
-- always using consistent data.

-- TODO: could mvar be polymorphic? (withMVar is OK for update, but coercion is needed for set)
mvar :: (Functor r, Applicative r, Monad r, IO !<! r) 
     => Simple (Reference IO r) (MVar a) a
mvar = reference (liftMS . readMVar)
                 (\newVal mv -> liftMS (putMVar mv newVal) >> return mv)     
                 (\trf mv -> liftMS (modifyMVar_ mv trf) >> return mv)     
          
-- | Access the value of an IORef.

-- TODO: could ioref be polymorphic?
ioref :: (Functor r, Applicative r, Monad r, IO !<! r) 
      => Simple (Reference IO r) (IORef a) a
ioref = reference (liftMS . readIORef) (\v ior -> liftMS (atomicWriteIORef ior v) >> return ior) 
                  (\trf ior -> liftMS (readIORef ior)
                                 >>= \v -> liftMS (trf v >>= writeIORef ior >> return ior)
                  ) 
  
-- | Access the state inside a state monad (from any context).
state :: ( Functor w, Applicative w, Monad w, MonadState s w
         , Functor r, Applicative r, Monad r, MonadState s r ) 
      => Simple (Reference w r) a s
state = reference (const get) (\a s -> put a >> return s) 
                  (\trf s -> (get >>= trf >> return s))   