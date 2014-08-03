{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}

-- | Predefined references.
-- 
-- _Naming convention_: If there is a reference @foo@ and a reference @foo'@ then 
-- @foo'@ is the restricted version of @foo@. If @foo@ is generic in it's writer monad
-- @foo'@ has the simplest writer monad that suffices.
module Control.Reference.Predefined where

import Control.Reference.Representation

import Control.Concurrent.MVar
import Data.IORef
import Data.Map as Map
import Data.Either.Combinators
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import qualified Control.Lens as Lens
import qualified Data.Traversable as Trav

-- * Trivial references

-- | An identical lens. Accesses the context.
simple :: Lens a b a b
simple = Reference return (const . return) id   

-- | An empty reference that do not traverse anything
emptyRef :: (MonadPlus m) => Reference m s s a a
emptyRef = Reference (const mzero) (const return) (const return)


-- * Reference generators

-- | Generates a traversal on any traversable
traverse :: (Trav.Traversable t, Monad m, Monoid (m a), MonadSubsume [] m)
            => Reference m (t a) (t b) a b
traverse = Reference (execWriter . Trav.mapM (tell . liftMS . (:[]))) 
                     (\v -> Trav.mapM (const $ return v)) 
                     Trav.mapM

-- | Generates a lens from a getter and a setter
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens get set = Reference (return . get) 
                         (\b -> return . set b ) 
                         (\f a -> f (get a) >>= \b -> return $ set b a)

-- | Creates a monomorphic partial lens
partial :: (s -> Maybe a) -> (a -> s -> s) -> LensPart s s a a
partial get set = Reference (liftMS . get)
                            (\b -> return . set b ) 
                            (\f a -> case get a of Just x -> f x >>= \b -> return $ set b a
                                                   Nothing -> return a)


-- | Creates a polymorphic partial lense
polyPartial :: forall s t a b .  (s -> Maybe (a, b -> t)) -> LensPart s t a b
polyPartial access = Reference (liftMS . fmap fst . access)
                               (\b s -> fmap (($ b) . snd) (liftMS (access s)))
                               (\f s -> (liftMS (access s)) >>= (\(v,set) -> f v >>= return . set))

-- | Generate a reference from a simple lens from 'Control.Lens'
fromLens :: Lens.Lens s s a a -> Lens.Lens s t a b -> Lens s t a b
fromLens lm lp = Reference (\s -> return (s Lens.^. lm)) 
                           (\b -> return . (lp Lens..~ b))
                           lp              
                           
-- | Generate a reference from a simple lens from 'Control.Lens'
fromTraversal :: Lens.Traversal s s a a -> Lens.Traversal s t a b -> Traversal s t a b
fromTraversal lm lp = Reference (\s -> liftMS (s Lens.^.. lm)) 
                                (\b -> return . (lp Lens..~ b))
                                lp
                                                           
-- | Filters the traversed elements with a given predicate. 
-- Has specific versions for traversals and partial lenses.
filtered :: (MonadPlus m) => (a -> Bool) -> Reference m a a a a
filtered p = Reference (\s -> if p s then return s else mzero)
                       (\a s -> if p s then return a else return s)
                       (\f s -> if p s then f s else return s)
                       
-- | Filters a traversal                       
filteredTrav :: (MonadPlus m) => (a -> Bool) -> Reference m a a a a
filteredTrav = filtered  
                              
-- | Filters a partial lens                       
filteredPartial :: (MonadPlus m) => (a -> Bool) -> Reference m a a a a
filteredPartial = filtered


-- | Generate a lens from a pair of inverse functions
iso :: (a -> b) -> (b -> a) -> Lens a a b b
iso f g = Reference (return . f) (\b _ -> return . g $ b) (\trf a -> trf (f a) >>= return . g  ) 

-- * References for simple data structures

-- TODO : change to partial lens generators

-- | A partial lens to access the value that may not exist
just :: LensPart (Maybe a) (Maybe b) a b
just = Reference liftMS (\v -> return . fmap (const v)) 
                        (\trf -> \case Just x -> liftM Just (trf x) 
                                       Nothing -> return Nothing)
             
-- | A partial lens to access the right option of an 'Either'
right :: LensPart (Either a b) (Either a c) b c
right = Reference (liftMS . rightToMaybe)
                  (\v -> return . mapRight (const v)) 
                  (\trf a -> case a of Right x -> liftM Right (trf x)
                                       Left y -> return (Left y) )    
                  
-- | A partial lens to access the left option of an 'Either'                  
left :: LensPart (Either a c) (Either b c) a b
left = Reference (liftMS . leftToMaybe)
                 (\v -> return . mapLeft (const v)) 
                 (\trf a -> case a of Left x -> liftM Left (trf x)
                                      Right y -> return (Right y) )


-- | Access the value that is in the left or right state of an 'Either'
anyway :: Lens (Either a a) (Either b b) a b
anyway = Reference (\case Left a -> return a; Right a -> return a)
                   (\b -> \case Left _ -> return (Left b); Right _ -> return (Right b))
                   (\f -> \case Left a -> f a >>= return . Left; Right a -> f a >>= return . Right)

-- | References both elements of a tuple
both :: Traversal (a,a) (b,b) a b
both = Reference (\(x,y) -> liftMS [x,y]) 
                 (\v -> return . const (v,v)) 
                 (\f (x,y) -> liftM2 (,) (f x) (f y))

-- | References the head of a list
_head :: LensPart [a] [a] a a
_head = Reference (\case x:_ -> return x; _ -> liftMS Nothing) 
                 (\a -> return . \case _:xs -> a:xs; [] -> []) 
                 (\f -> \case x:xs -> liftM (:xs) (f x); [] -> return [])     
    
-- | References the tail of a list
_tail :: LensPart [a] [a] [a] [a]
_tail = Reference (\case _:xs -> return xs; _ -> liftMS Nothing) 
                  (\ys -> return . \case x:_ -> x:ys; [] -> []) 
                  (\f -> \case x:xs -> liftM (x:) (f xs); [] -> return [])
                 
-- | Lenses for given values in a data structure that is indexed by keys.
class Association e where
  type AssocIndex e :: *
  type AssocElem e :: *
  element :: AssocIndex e -> LensPart e e (AssocElem e) (AssocElem e)
          
instance Association [a] where          
  type AssocIndex [a] = Int
  type AssocElem [a] = a
  element i = Reference (liftMS . at i) (\v -> upd (const (return v)))
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
  element k = Reference (liftMS . Map.lookup k)
                        (\v -> return . insert k v) 
                        (\trf m -> case Map.lookup k m of Just x -> trf x >>= \x' -> return (insert k x' m)
                                                          Nothing -> return m)

-- * Stateful references
                                                          
-- | Access a value inside an MVar. Writing should only be used for initial 
-- assignment or parts of the program will block infinitely. Reads and updates are done in sequence,
-- always using consistent data.

-- TODO: could mvar be polymorphic? (withMVar is OK for update, but coercion is needed for set)
mvar :: RefIO (MVar a) (MVar a) a a
mvar = Reference (liftMS . readMVar)
                 (\newVal mv -> liftMS (putMVar mv newVal) >> return mv)     
                 (\trf mv -> liftMS (modifyMVar_ mv (summarizeFor trf)) >> return mv)     

-- | Access the current value inside an MVar. Never blocks.
--mvarNow :: Reference (MaybeT IO) (MVar a) (MVar a) a a
--mvarNow = Reference (MaybeT . tryTakeMVar)
--                    (\newVal mv -> MaybeT (tryPutMVar mv newVal
--                                             >>= \b -> return (if b then Just mv
--                                                                    else Nothing)))  
--                    (\trf mv -> MaybeT (tryTakeMVar mv >>= return . fmap (\x -> trf x >>= tryPutMVar mv)))

          
-- | Access the value of an IORef.

-- TODO: could ioref be polymorphic?
ioref :: RefIO (IORef v) (IORef v) v v
ioref = Reference (liftMS . readIORef) (\v ior -> liftMS (atomicWriteIORef ior v) >> return ior) 
                  (\trf ior -> liftMS (readIORef ior)
                                 >>= \v -> liftMS (summarizeFor trf v >>= writeIORef ior >> return ior)
                  ) 
  
-- | Access the state inside a state monad (from any context).
state :: (MonadState s m) => Reference m a a s s
state = Reference (const get) (\a s -> put a >> return s) 
                  (\trf s -> (get >>= trf >> return s))   
