{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase, TupleSections, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif


-- | Predefined references for commonly used data structures and reference generators.
--
-- When defining lenses one should use the more general types. For instance 'Lens' instead of the more strict 'Lens''. This way references with different @m1@ and @m2@ monads can be combined if there is a monad @m'@ for @MMorph m1 m'@ and @MMorph m2 m'@.
module Control.Reference.Predefined where

import Control.Reference.Representation
import Control.Reference.Operators

import Control.Applicative
import Control.Monad
import qualified Data.Traversable as Trav
import Data.Ratio
import Control.Monad.Trans.Control
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan
import Data.IORef
import Data.Either.Combinators

-- * Trivial references

-- | An identical lens. Accesses the context.
--
-- > self & a = a & self = a
self :: Lens a b a b
self = reference return (const . return) id   

-- | An empty reference that do not traverse anything
--
-- > emptyRef &+& a = a &+& emptyRef = a
--
-- > a & emptyRef = emptyRef & a = emptyRef
emptyRef :: Simple RefPlus s a
emptyRef = reference (const mzero) (const return) (const return)


-- * Reference generators

-- | Generates a traversal for any 'Trav.Traversable' 'Functor'
traverse :: (Trav.Traversable t) => Traversal (t a) (t b) a b
traverse = reference (morph . execWriter . Trav.mapM (tell . (:[])))
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
partial :: (s -> Either t (a, b -> t)) -> Partial s t a b
partial access 
  = reference 
      (\s   -> case access s of Left _ -> morph Nothing
                                Right (a,_) -> return a)
      (\b s -> case access s of Left t -> return t
                                Right (_,set) -> return (set b))
      (\f s -> case access s of Left t -> return t
                                Right (a,set) -> f a >>= return . set)

-- | Creates a simple partial lens
simplePartial :: (s -> Maybe (a, a -> s)) -> Partial s s a a
simplePartial access 
  = partial (\s -> case access s of Just x -> Right x
                                    Nothing -> Left s)

                                                     
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

-- * References for simple data structures

-- | A partial lens to access the value that may not exist
just :: Partial (Maybe a) (Maybe b) a b
just = partial (\case Just x -> Right (x, Just)
                      Nothing -> Left Nothing)

-- | A partial lens to access the right option of an 'Either'
right :: Partial (Either a b) (Either a c) b c
right = partial (\case Right x -> Right (x, Right)
                       Left a -> Left (Left a))

-- | A partial lens to access the left option of an 'Either'                  
left :: Partial (Either a c) (Either b c) a b
left = partial (\case Left a -> Right (a, Left)
                      Right r -> Left (Right r))

-- | Access the value that is in the left or right state of an 'Either'
anyway :: Lens (Either a a) (Either b b) a b
anyway = reference (either return return)
                   (\b -> return . mapBoth (const b) (const b))
                   (\f -> either (f >=> return . Left) (f >=> return . Right))

-- | References both elements of a tuple
both :: Traversal (a,a) (b,b) a b
both = reference (\(x,y) -> morph [x,y]) 
                 (\v -> return . const (v,v)) 
                 (\f (x,y) -> (,) <$> f x <*> f y)

-- | References the head of a list
_head :: Simple Partial [a] a
_head = simplePartial (\case [] -> Nothing; x:xs -> Just (x,(:xs)))
    
-- | References the tail of a list
_tail :: Simple Partial [a] [a]
_tail = simplePartial (\case [] -> Nothing; x:xs -> Just (xs,(x:)))
           
-- | Accesses the numerator of a ratio
_numerator :: Integral a => Simple Lens (Ratio a) a
_numerator = lens numerator (\num' r -> num' % denominator r) 

-- | Accesses the denominator of a ratio
_denominator :: Integral a => Simple Lens (Ratio a) a
_denominator = lens denominator (\denom' r -> numerator r % denom') 
           
-- * Stateful references

-- | A dummy object to interact with the user through the console.
data Console = Console

-- | Interacts with a line of text on the console. Values set are printed, getting
-- is reading from the console.
consoleLine :: Simple IOLens Console String
consoleLine 
  = reference (const (morph getLine)) 
              (\str -> const (morph (putStrLn str) >> return Console)) 
              (\f -> const (morph getLine >>= f 
                                           >>= morph . putStrLn 
                                           >> return Console))

               
-- | Access a value inside an MVar.
-- Setting is not atomic. If there is two supplier that may set the accessed
-- value, one may block and can corrupt the following updates.
--
-- Reads and updates are done in sequence, always using consistent data.
mvar :: Simple IOLens (MVar a) a
mvar = reference (morph . (readMVar :: MVar a -> IO a))
                 (\newVal mv -> do empty <- isEmptyMVar mv
                                   when empty (swapMVar mv newVal >> return ())
                                   return mv)
                 (\trf mv -> modifyMVarMasked_ mv trf >> return mv)     


chan :: Simple IOLens (Chan a) a
chan = reference (morph . readChan)
                 (\a ch -> morph (writeChan ch a) >> return ch)
                 (\trf ch -> morph (readChan ch) >>= trf
                               >>= morph . writeChan ch >> return ch)
       
-- | Access the value of an IORef. 
ioref :: Simple IOLens (IORef a) a
ioref = reference (morph . readIORef)
                  (\v ior -> morph (atomicWriteIORef ior v) >> return ior) 
                  (\trf ior -> morph (readIORef ior)
                                 >>= trf >>= morph . writeIORef ior >> return ior) 
        
-- | Access the state inside a state monad (from any context).
state :: forall s m a . Monad m => Simple (StateLens s m) a s
state = reference (morph . const get') (\a s -> morph (put' a) >> return s) 
                  (\trf s -> (morph get' >>= trf >> return s))
  where put' = put :: s -> StateT s m ()
        get' = get :: StateT s m s
