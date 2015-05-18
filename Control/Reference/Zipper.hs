{-# LANGUAGE TypeOperators, RankNTypes, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, GADTs #-}
module Control.Reference.Zipper where

import Control.Monad.Identity
import Control.Reference.Representation
import Control.Reference.Types
import Control.Reference.Predefined
import Control.Reference.Operators
import Control.Reference.Combinators
import Control.Reference.TupleInstances

data Zipper h (w :: * -> *) (r :: * -> *) s t a b 
  = Zipper { zipperFocus :: h w r s t a b
           , zipperWhole :: s
           }

data (:>) h x y w r s t a b = h w r s t x y :> Reference w r MU MU s t a b

data Top (w :: * -> *) (r :: * -> *) a b c d where
  Top :: Top w r a b a b

type SndZip w r = (:>) Top (Int,String) (Int,String) w r (Int,String) (Int,String) String String
sz :: SndZip Identity Identity
sz = Top :> _2

type SndSndZip w r = (:>) ((:>) Top (Int,(Int,String)) (Int,(Int,String))) (Int,String) (Int,String) w r (Int,(Int,String)) (Int,(Int,String)) String String

ssz :: SndSndZip Identity Identity
ssz = Top :> _2 :> (_2&_2)

test = let zippo = move _2 $ move _2 $ zipper (2::Int, (1::Int, "almafa")) 
        in zipRef zippo .= "b" $ zipperWhole zippo

class HistoryLens h where
  historyLens :: RefMonads w r => h w r s t a b -> Reference w r MU MU s t a b
instance HistoryLens Top where
  historyLens Top = self
instance HistoryLens ((:>) h x y) where
  historyLens (_ :> r) = r


zipper :: RefMonads w r => a -> Zipper Top r w a b a b
zipper = Zipper Top

close :: Zipper Top r w a b a b -> a
close (Zipper Top a) = a

zipRef :: (RefMonads w r, HistoryLens h) => Zipper h w r s t a b -> Reference w r MU MU s t a b
zipRef z = historyLens (zipperFocus z)

setFocus :: b -> Zipper h w r s t a b -> Zipper h w r t t b b 
setFocus v z = z { zipperWhole = zipRef z .= v $ zipperWhole z }

move :: (RefMonads w r, HistoryLens h) 
     => Reference w r MU MU a b c d 
     -> Zipper h w r s t a b 
     -> Zipper ((:>) h a b) w r s t c d
move l (Zipper h s) = Zipper (h :> (historyLens h & l)) s

moveUp :: Zipper ((:>) h a b) w r s t c d -> Zipper h w r s t a b
moveUp (Zipper (h :> _) s) = Zipper h s

