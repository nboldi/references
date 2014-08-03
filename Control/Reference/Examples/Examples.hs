{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of random example references
module Control.Reference.Examples.Examples where

import Control.Reference

import qualified Control.Lens as Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Language.Haskell.TH

test1 = just ?= 3 $ Nothing

test2 :: Either a Int
test2 = right ?= 3 $ Right 2

test3 :: Either a Int
test3 = right ?- (+1) $ Right 2

test4 :: Either a (Maybe Int)
test4 = right&just ?- (+1) $ Right (Just 2)

test5 :: Either a (Maybe [Int])
test5 = right&just&(element 3) ?- (+1) $ Right (Just [1..10])

test6 :: (Int, Int)
test6 = both *- (+1) $ (0, 1)

test7 :: (Maybe Int, Maybe Int)
test7 = both&just *- (+1) $ (Just 0, Nothing)

-- should not block
test8 :: IO (MVar Int)
test8 = newEmptyMVar >>= emptyRef&mvar ?!- (+1)

test9 = let isoList = iso length (`replicate` ())
         in isoList .- (+1) $ [(),(),()]

test10 :: [Int]
test10 = [1..10] ^* _tail&traverse &+& _tail&_tail&traverse

test11 :: [Int]
test11 = _tail&traverse &+& _tail&_tail&traverse *- (+1) $ replicate 10 1

test12 :: IO (Int,Int)
test12 = both !| print $ (0, 1)
    
data Dept = Dept { _manager :: Employee
                 , _staff :: [Employee] 
                 } deriving Show
data Employee = Employee { __name :: String
                         , __salary :: Float
                         } deriving Show
                         
$(Lens.makeLenses ''Employee)
                         
manager :: Lens Dept Dept Employee Employee
manager = lens _manager (\b a -> a { _manager = b })

staff :: Lens Dept Dept [Employee] [Employee]
staff = lens _staff (\b a -> a { _staff = b })  
                       
name :: Lens Employee Employee String String
name = fromLens _name _name

salary :: Lens Employee Employee Float Float
salary = fromLens _salary _salary
                                     
dept = Dept (Employee "Agamemnon" 100000) [Employee "Akhilles" 30000, Employee "Menelaos" 40000]

test13 = manager&salary .- (*2) $ dept

test14 = traverse *- (`replicate` 'x') $ [1..10]

test15 = let lens_1 = fromLens Lens._1 Lens._1
          in lens_1 .- show $ (2,'a')
test16 = (_1 &+& _2) & (left &+& right) ?- ((+1) :: Int -> Int) 
           $ (Left 3 :: Either Int Int, Right 1 :: Either Int Int)

data PWrapped m a = PWrapped { _pwrap :: m a }

pwrap :: Lens (PWrapped m a) (PWrapped n b) (m a) (n b)
pwrap = lens (\(PWrapped a) -> a) (\a _ -> PWrapped a)

data MWrapped a = MWrapped { _mwrap :: Maybe a }
mwrap :: Lens (MWrapped a) (MWrapped b) (Maybe a) (Maybe b)
mwrap = lens (\(MWrapped a) -> a) (\a _ -> MWrapped a)

data Maybe' a = Just' { _fromJust' :: a }
              | Nothing'
              
fromJust' :: LensPart (Maybe' a) (Maybe' b) a b
fromJust' = polyPartial (\case Just' x -> Just (x, Just')
                               Nothing' -> Nothing)
    
data Tuple a b = Tuple { _fst' :: a, _snd' :: b }
         
fst' :: Lens (Tuple a c) (Tuple b c) a b
fst' = lens _fst' (\b tup -> tup { _fst' = b })
                
test = 
  do result <- newEmptyMVar
     terminator <- newEmptyMVar
     forkIO $ (result ^! mvar) >>= print >> (mvar != ()) terminator >> return ()
     hello <- newMVar (Just "World")
     forkIO $ ((mvar&just&_tail&_tail) ?!- ('_':) $ hello) >> return ()
     forkIO $ ((mvar&just&(element 1)) ?!= 'u' $ hello) >> return ()
     forkIO $ ((mvar&just) ?!- ("Hello" ++) $ hello) >> return ()
     
     x <- runMaybeT $ hello ^?! (mvar & just) 
     mvar != x $ result
     terminator ^! mvar
