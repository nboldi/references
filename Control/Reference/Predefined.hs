{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}

-- | Predefined references.
-- 
-- _Naming convention_: If there is a reference @foo@ and a reference @foo'@ then 
-- @foo'@ is the restricted version of @foo@. If @foo@ is generic in it's writer monad
-- @foo'@ has the simplest writer monad that suffices.
module Control.Reference.Predefined where

import Control.Reference.Representation
import Control.Reference.Operators
import Control.Reference.TH.Tuple

import Control.Concurrent.MVar
import Data.IORef
import Data.Map as Map
import Data.Maybe
import Data.Either.Combinators
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Control.Lens as Lens
import qualified Data.Traversable as Trav

-- * Trivial references
             
-- | An identical lens. Accesses the context.
simple :: Monad w => Lens' w a b a b
simple = Reference return (const . return) id   
     
simple' :: Lens a b a b
simple' = simple

-- | An empty reference that do not traverse anything
emptyRef :: (Monad w, Monad r, MonadPlus r) => SimpleRef w r s a
emptyRef = Reference (const mzero) (const return) (const return)

emptyRef' :: (Monad w) => SimpleRef w Maybe s a
emptyRef' = emptyRef

-- * Reference generators

-- | Generates a traversal on any traversable
traverse :: (Monad w, Trav.Traversable t) => Traversal' w (t a) (t b) a b
traverse = Reference (execWriter . Trav.mapM (tell . (:[]))) 
                     (\v -> Trav.mapM (const $ return v)) 
                     Trav.mapM
               
traverse' :: (Trav.Traversable t) => Traversal (t a) (t b) a b
traverse' = traverse


-- | Generates a lens from a getter and a setter
lens :: Monad w => (s -> a) -> (b -> s -> t) -> Lens' w s t a b
lens get set = Reference (return . get) 
                         (\b -> return . set b ) 
                         (\f a -> f (get a) >>= \b -> return $ set b a)
                     
lens' :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens' = lens

-- | Creates a monomorphic partial lens
partial :: Monad w => (s -> Maybe a) -> (a -> s -> s) -> Simple' w LensPart' s a
partial get set = Reference get
                            (\b -> return . set b ) 
                            (\f a -> case get a of Just x -> f x >>= \b -> return $ set b a
                                                   Nothing -> return a)
                     
partial' :: (s -> Maybe a) -> (a -> s -> s) -> Simple LensPart s a
partial' = partial

-- | Creates a polymorphic partial lense
polyPartial :: Monad w => (s -> Either (w t) (a, b -> w t)) -> LensPart' w s t a b
polyPartial gets = Reference (fmap fst . rightToMaybe . gets)
                             (\b s -> case gets s of Right (_, set) -> set b
                                                     Left t -> t ) 
                             (\f a -> case gets a of Right (x, set) -> f x >>= set
                                                     Left t -> t )
                     
polyPartial' :: (s -> Either t (a, b -> t)) -> LensPart s t a b
polyPartial' gets = polyPartial (\s -> case gets s of Left t -> Left (return t)
                                                      Right (v,set) -> Right (v, return . set))


                     
-- | Generate a reference from a simple lens from 'Control.Lens'
fromLens :: (Functor w, Monad w) => Lens.Lens s s a a -> Lens.Lens s t a b -> Lens' w s t a b
fromLens lm lp = Reference (\s -> return (s Lens.^. lm)) 
                           (\b -> return . (lp Lens..~ b))
                           lp              
                           
-- | Generate a reference from a simple lens from 'Control.Lens'
fromTraversal :: (Applicative w, Monad w) 
              => Lens.Traversal s s a a -> Lens.Traversal s t a b -> Traversal' w s t a b
fromTraversal lm lp = Reference (\s -> s Lens.^.. lm) 
                                (\b -> return . (lp Lens..~ b))
                                lp
                                                           
-- | Filters the traversed elements with a given predicate. 
-- Has specific versions for traversals and partial lenses.
filtered :: (Applicative w, Monad w, MonadPlus r) 
         => (a -> Bool) -> SimpleRef w r a a
filtered pred = Reference (\s -> if pred s then return s else mzero)
                          (\a s -> if pred s then return a else return s)
                          (\f s -> if pred s then f s else return s)
                       
-- | Filters a traversal                       
filteredTrav :: (Applicative w, Monad w) => (a -> Bool) -> Simple' w Traversal' a a
filteredTrav = filtered  
                              
-- | Filters a partial lens                       
filteredPartial :: (Applicative w, Monad w) => (a -> Bool) -> Simple' w LensPart' a a
filteredPartial = filtered


-- | Generate a lens from a pair of inverse functions
iso :: Monad w => (a -> b) -> (b -> a) -> Simple' w Lens' a b
iso f g = Reference (return . f) (\b _ -> return . g $ b) (\trf a -> trf (f a) >>= return . g  )      
             
iso' :: (a -> b) -> (b -> a) -> Simple Lens a b
iso' = iso

-- * References for simple data structures

-- TODO : change to partial lens generators

-- | A partial lens to access the value that may not exist
just :: Monad w => LensPart' w (Maybe a) (Maybe b) a b
just = Reference id (\v -> return . fmap (const v)) 
                    (\trf -> \case Just x -> liftM Just (trf x) 
                                   Nothing -> return Nothing)
                              
just' :: LensPart (Maybe a) (Maybe b) a b
just' = just
             
-- | A partial lens to access the right option of an 'Either'
right :: Monad w => LensPart' w (Either a b) (Either a c) b c
right = Reference rightToMaybe (\v -> return . mapRight (const v)) 
                  (\trf a -> case a of Right x -> liftM Right (trf x)
                                       Left y -> return (Left y) )    
                                  
right' :: LensPart (Either a b) (Either a c) b c
right' = right
                  
-- | A partial lens to access the left option of an 'Either'                  
left :: Monad w => LensPart' w (Either a c) (Either b c) a b
left = Reference leftToMaybe (\v -> return . mapLeft (const v)) 
                 (\trf a -> case a of Left x -> liftM Left (trf x)
                                      Right y -> return (Right y) )
                                                    
left' :: LensPart (Either a c) (Either b c) a b
left' = left

-- | Access the value that is in the left or right state of an 'Either'
anyway :: Monad w => Lens' w (Either a a) (Either b b) a b
anyway = Reference (\case Left a -> return a; Right a -> return a)
                   (\b -> \case Left _ -> return (Left b); Right _ -> return (Right b))
                   (\f -> \case Left a -> f a >>= return . Left; Right a -> f a >>= return . Right)

anyway' :: Lens (Either a a) (Either b b) a b
anyway' = anyway

-- | References both elements of a tuple
both :: Monad w => Traversal' w (a,a) (b,b) a b
both = Reference (\(x,y) -> [x,y]) 
                 (\v -> return . const (v,v)) 
                 (\f (x,y) -> liftM2 (,) (f x) (f y))

both' :: Traversal (a,a) (b,b) a b
both' = both

-- | References the head of a list
_head :: Monad w => Simple' w LensPart' [a] a
_head = Reference (\case x:_ -> Just x; _ -> Nothing) 
                 (\a -> return . \case _:xs -> a:xs; [] -> []) 
                 (\f -> \case x:xs -> liftM (:xs) (f x); [] -> return [])     
    
_head' :: Simple LensPart [a] a
_head' = _head
    
-- | References the tail of a list
_tail :: Monad w => Simple' w LensPart' [a] [a]
_tail = Reference (\case _:xs -> Just xs; _ -> Nothing) 
                  (\ys -> return . \case x:_ -> x:ys; [] -> []) 
                  (\f -> \case x:xs -> liftM (x:) (f xs); [] -> return [])
                  
_tail' :: Simple LensPart [a] [a]
_tail' = _tail
                 
-- | Lenses for given values in a data structure that is indexed by keys.
class Association e where
  type AssocIndex e :: *
  type AssocElem e :: *
  element :: Monad w => AssocIndex e -> Simple' w LensPart' e (AssocElem e)
  
  element' :: AssocIndex e -> Simple LensPart e (AssocElem e)
  element' = element
          
instance Association [a] where          
  type AssocIndex [a] = Int
  type AssocElem [a] = a
  element i = Reference (at i) (\v -> update (const (return v)))
                        update
    where at :: Int -> [a] -> Maybe a
          at n xs | n < 0 =  Nothing
          at _ []         =  Nothing
          at 0 (x:_)      =  Just x
          at n (_:xs)     =  at (n-1) xs
          
          update :: Monad w => (a -> w a) -> [a] -> w [a]
          update f ls = let (before,rest) = splitAt i ls
                         in case rest of [] -> return before
                                         (x:xs) -> f x >>= \fx -> return $ before ++ fx : xs
  
instance Ord k => Association (Map k v) where
  type AssocIndex (Map k v) = k
  type AssocElem (Map k v) = v
  element k = Reference (Map.lookup k) (\v -> return . insert k v) 
                        (\trf m -> case Map.lookup k m of Just x -> return (insert k x m)
                                                          Nothing -> return m)

-- * Stateful references
                                                          
-- | Access a value inside an MVar. Writing should only be used for initial 
-- assignment or parts of the program will block infinitely. Reads and updates are done in sequence,
-- always using consistent data.

-- TODO: could mvar be polymorphic? (withMVar is OK for update, but coercion is needed for set)
mvar :: SimpleRef IO IO (MVar a) a
mvar = Reference readMVar
                 (\newVal mv -> putMVar mv newVal >> return mv)     
                 (\trf mv -> modifyMVar_ mv trf >> return mv)     

-- | Access the current value inside an MVar. Never blocks.
mvarNow :: SimpleRef IO (MaybeT IO) (MVar a) a
mvarNow = Reference (MaybeT . tryTakeMVar)
                    (\newVal mv -> tryPutMVar mv newVal >> return mv)     
                    (\trf mv -> tryTakeMVar mv >>= \case Just x -> trf x >>= tryPutMVar mv >> return mv
                                                         Nothing -> return mv)  
                 
-- | Access the value of an IORef.

-- TODO: could ioref be polymorphic?
ioref :: SimpleRef IO IO (IORef a) a
ioref = Reference readIORef (\v ior -> atomicWriteIORef ior v >> return ior) 
                  (\trf ior -> readIORef ior >>= trf >>= writeIORef ior >> return ior) 
  
-- | Access the state inside a state monad (from any context).
state :: SimpleRef (State s) (State s) a s
state = Reference (const get) (\a s -> put a >> return s) 
                  (\trf s -> (get >>= trf >> return s))   
