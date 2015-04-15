{-# LANGUAGE TypeOperators, RankNTypes, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, GADTs #-}
module Control.Reference.Zipper where

import Control.Monad.Identity
import Control.Reference.Representation
import Control.Reference.Types
import Control.Reference.Predefined
import Control.Reference.Operators
import Control.Reference.Combinators
import Control.Reference.TupleInstances

data Zipper h s t a b = Zipper { zipperFocus :: h s t a b
                               , zipperWhole :: s
                               }

data (:>) h x y s t a b = h s t x y :> Reference Identity Identity MU MU s t a b

data Top a b c d where
  Top :: Top a b a b

type SndZip = (:>) Top (Int,String) (Int,String) (Int,String) (Int,String) String String
sz :: SndZip
sz = Top :> _2

type SndSndZip = (:>) ((:>) Top (Int,(Int,String)) (Int,(Int,String))) (Int,String) (Int,String) (Int,(Int,String)) (Int,(Int,String)) String String

ssz :: SndSndZip
ssz = Top :> _2 :> (_2&_2)

test = move _2 $ move _2 $ zipper (2::Int, (1::Int, "almafa")) 

class HistoryLens h where
  historyLens :: h s t a b -> Reference Identity Identity MU MU s t a b
instance HistoryLens Top where
  historyLens Top = self
instance HistoryLens ((:>) h x y) where
  historyLens (_ :> r) = r


zipper :: a -> Zipper Top a b a b
zipper = Zipper Top

close :: Zipper Top a b a b -> a
close (Zipper Top a) = a

move :: HistoryLens h => Lens a b c d -> Zipper h s t a b -> Zipper ((:>) h a b) s t c d
move l (Zipper h s) = Zipper (h :> (historyLens h & l)) s

moveUp :: Zipper ((:>) h a b) s t c d -> Zipper h s t a b
moveUp (Zipper (h :> _) s) = Zipper h s

