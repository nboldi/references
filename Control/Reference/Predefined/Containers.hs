{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
module Control.Reference.Predefined.Containers where

import Control.Reference.Representation
import Control.Reference.Predefined
import Control.Reference.Operators
                 
import Data.Map as Map
import qualified Data.Array as Arr
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
                 
-- | Lenses for given values in a data structure that is indexed by keys.
class Association e where
  type AssocIndex e :: *
  type AssocElem e :: *
  
  element :: AssocIndex e -> Simple Partial e (AssocElem e)
     
instance Association [a] where          
  type AssocIndex [a] = Int
  type AssocElem [a] = a
  element i = reference (morph . at i) (\v -> upd (const (return v))) upd
    where at :: Int -> [a] -> Maybe a
          at n _ | n < 0  = Nothing
          at _ []         = Nothing
          at 0 (x:_)      = Just x
          at n (_:xs)     = at (n-1) xs
          
          upd :: Monad w => (a -> w a) -> [a] -> w [a]
          upd f ls = let (before,rest) = splitAt i ls
                      in case rest of [] -> return before
                                      (x:xs) -> f x >>= \fx -> return $ before ++ fx : xs
   
instance Arr.Ix i => Association (Arr.Array i a) where          
  type AssocIndex (Arr.Array i a) = i
  type AssocElem (Arr.Array i a) = a
  element i = reference (morph . at) (\v -> upd (const (return v))) upd
    where at :: (Arr.Array i a) -> Maybe a
          at arr | Arr.inRange (Arr.bounds arr) i
                 = Just (arr Arr.! i)
             | otherwise = Nothing
          upd :: Monad w => (a -> w a) -> Arr.Array i a -> w (Arr.Array i a)
          upd f arr | Arr.inRange (Arr.bounds arr) i
                    = f (arr Arr.! i) >>= \v -> return (arr Arr.// [(i,v)])
             | otherwise = return arr

instance Association (Seq.Seq a) where          
  type AssocIndex (Seq.Seq a) = Int
  type AssocElem (Seq.Seq a) = a
  element i = reference (morph . at i) (\v -> upd (const (return v)))
                        upd
    where at :: Int -> Seq.Seq a -> Maybe a
          at n s = case Seq.viewl (snd (Seq.splitAt i s)) of 
                     Seq.EmptyL -> Nothing
                     v Seq.:< _ -> Just v
          
          upd :: Monad w => (a -> w a) -> Seq.Seq a -> w (Seq.Seq a)
          upd f s = let (before,rest) = Seq.splitAt i s
                     in case Seq.viewl rest of 
                          Seq.EmptyL -> return before
                          x Seq.:< xs -> f x >>= \fx -> return $ before Seq.>< (fx Seq.<| xs)
  
class Association e => Mapping e where
  at :: AssocIndex e -> Simple Lens e (Maybe (AssocElem e))
    
instance Eq a => Association (a -> Maybe b) where          
  type AssocIndex (a -> Maybe b) = a
  type AssocElem (a -> Maybe b) = b
  element i = simplePartial (\f -> case f i of Just x -> Just (x, \b k -> if i == k then Just b else f k)
                                               Nothing -> Nothing) 
                                               
instance Eq a => Mapping (a -> Maybe b) where
  at i = lens ($ i) (\b f k -> if i == k then b else f k)
    
instance Ord k => Association (Map k v) where
  type AssocIndex (Map k v) = k
  type AssocElem (Map k v) = v
  element k = reference (morph . Map.lookup k)
                        (\v -> return . Map.insert k v) 
                        (\trf m -> case Map.lookup k m of Just x -> trf x >>= \x' -> return (Map.insert k x' m)
                                                          Nothing -> return m)

instance Ord k => Mapping (Map k v) where
  at k = reference (return . (^? element k))
                   (\v -> return . Map.alter (const v) k) 
                   (\f m -> f (Map.lookup k m) >>=
                              return . maybe (Map.delete k m) 
                                             (\fx -> Map.insert k fx m))                   
                                                          
instance Association (IM.IntMap v) where
  type AssocIndex (IM.IntMap v) = Int
  type AssocElem (IM.IntMap v) = v
  element k = reference (morph . IM.lookup k)
                        (\v -> return . IM.insert k v) 
                        (\trf m -> case IM.lookup k m of 
                                     Just x -> trf x >>= \x' -> return (IM.insert k x' m)
                                     Nothing -> return m)

instance Mapping (IM.IntMap v) where
  at k = reference (return . (^? element k))
                   (\v -> return . IM.alter (const v) k) 
                   (\f m -> f (IM.lookup k m) >>=
                              return . maybe (IM.delete k m) 
                                             (\fx -> IM.insert k fx m))   
                                                         
-- | Containers that can be used as a set, inserting and removing elements
class SetLike e where
  type SetElem e :: *
  contains :: (SetElem e) -> Simple Lens e Bool
                
instance Ord v => SetLike (Set.Set v) where
  type SetElem (Set.Set v) = v
  contains e 
    = reference 
        (return . Set.member e)
        (\v -> return . if v then Set.insert e
                             else Set.delete e)
        (\trf s -> trf (Set.member e s) >>= return . \case  
                     True -> Set.insert e s
                     False -> Set.delete e s)
                     
instance SetLike IS.IntSet where
  type SetElem IS.IntSet = Int
  contains e 
    = reference 
        (return . IS.member e)
        (\v -> return . if v then IS.insert e
                             else IS.delete e)
        (\trf s -> trf (IS.member e s) >>= return . \case  
                     True -> IS.insert e s
                     False -> IS.delete e s)