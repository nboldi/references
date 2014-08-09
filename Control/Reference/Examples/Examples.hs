{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- | A collection of random example references
module Control.Reference.Examples.Examples where

import Control.Reference

import qualified Control.Lens as Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Writer
import Language.Haskell.TH hiding (ListT)

import Test.HUnit


data A = A deriving (Eq, Show)

test1 :: Maybe Int
test1 = just ?= 3 $ Nothing

test2 :: Either A Int
test2 = right ?= 3 $ Right 2

test3 :: Either A Int
test3 = right ?- (+1) $ Right 2

test4 :: Either A (Maybe Int)
test4 = right&just ?- (+1) $ Right (Just 2)

test5 :: Either A (Maybe [Int])
test5 = right&just&(element 3) ?- (+1) $ Right (Just [1..10])

test6 :: (Int, Int)
test6 = both *- (+1) $ (0, 1)

test7 :: (Maybe Int, Maybe Int)
test7 = both&just *- (+1) $ (Just 0, Nothing)

-- should not block
test8 :: IO (MVar Int)
test8 = newEmptyMVar >>= emptyRef&mvar ?!- (+1)

test9 :: [()]
test9 = let isoList = iso length (`replicate` ())
         in isoList .- (+1) $ [(),(),()]

test10 :: [Int]
test10 = [1..10] ^* _tail&traverse &+& _tail&_tail&traverse

test11 :: [Int]
test11 = _tail&traverse &+& _tail&_tail&traverse *- (+1) $ replicate 10 1

test12 :: ListT (Writer [String]) (Int,Int)
test12 = both #| (lift . tell . (:[]) . show) $ (0, 1)

instance Monoid s => [] !<! (ListT (Writer s)) where
  liftMS = ListT . return 
         
data Dept = Dept { _manager :: Employee
                 , _staff :: [Employee] 
                 } deriving (Eq, Show)
data Employee = Employee { __name :: String
                         , __salary :: Float
                         } deriving (Eq, Show)
                         
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

test13 :: ListT (Writer (Sum Float)) Dept
test13 = ((staff&traverse &+& manager)&salary #| lift . tell . Sum)
           $ manager&name .- ("Mr. "++)
           $ dept

test14 :: [String]
test14 = traverse $*- (`replicate` 'x') $ [1..10]

test15 :: (String, Char)
test15 = let lens_1 = fromLens Lens._1 Lens._1
          in lens_1 .- show $ (2,'a')

test16 :: (Either Int Int, Either Int Int)
test16 = (_1 &+& _2) & (left &+& right) *- (+1)
           $ both & anyway *- subtract 1
           $ (Left 3, Right 1)

data PWrapped m a = PWrapped { _pwrap :: m a } deriving (Eq, Show)

$(makeReferences ''PWrapped)

test17 :: PWrapped Maybe String
test17 = pwrap .- (return . show . runIdentity) $ (PWrapped (Identity (3 :: Int)))

data MWrapped a = MWrapped { _mwrap :: Maybe a } deriving (Eq, Show)

$(makeReferences ''MWrapped)

test18 :: MWrapped String
test18 = mwrap .- (fmap show) $ MWrapped (Just (3 :: Int))


data Maybe' a = Just' { _fromJust' :: a }
              | Nothing'
              deriving (Eq, Show)
              
$(makeReferences ''Maybe')

test19 :: Maybe' String
test19 = fromJust' $?- show $ Just' (42 :: Int)
    
data Tuple a b = Tuple { _fst' :: a, _snd' :: b } deriving (Eq, Show)
         
$(makeReferences ''Tuple)

test20 :: Tuple Int String 
test20 = fst' .- length
         $ snd' .- show
         $ Tuple "almafa" 42

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


tests = TestList [ TestCase $ assertEqual "test1" Nothing test1
                 , TestCase $ assertEqual "test2" (Right 3) test2
                 , TestCase $ assertEqual "test3" (Right 3) test3
                 , TestCase $ assertEqual "test4" (Right (Just 3)) test4
                 , TestCase $ assertEqual "test5" (Right (Just [1,2,3,5,5,6,7,8,9,10])) test5
                 , TestCase $ assertEqual "test6" (1,2) test6
                 , TestCase $ assertEqual "test7" (Just 1,Nothing) test7
                 , TestCase $ assertBool "test8" =<< ((==) <$> (newEmptyMVar >>= tryTakeMVar)
                                                           <*> (test8 >>= tryTakeMVar))
                 , TestCase $ assertEqual "test9" [(),(),(),()] test9
                 , TestCase $ assertEqual "test10" ([2..10]++[3..10]) test10
                 , TestCase $ assertEqual "test11" ([1,2]++replicate 8 3) test11
                 , TestCase $ assertEqual "test12" ["0","1"] (execWriter (runListT test12))
                 , TestCase $ assertEqual "test13" (dept { _manager = Employee "Mr. Agamemnon" 100000 }, Sum 170000)
                                                   (runWriter (head <$> runListT test13))
                 , TestCase $ assertEqual "test14" [replicate i 'x' | i <- [1..10]] test14
                 , TestCase $ assertEqual "test15" ("2",'a') test15
                 , TestCase $ assertEqual "test16" (Left 3, Right 1) test16
                 , TestCase $ assertEqual "test17" (PWrapped (Just "3")) test17
                 , TestCase $ assertEqual "test18" (MWrapped (Just "3")) test18
                 , TestCase $ assertEqual "test19" (Just' "42") test19
                 , TestCase $ assertEqual "test20" (Tuple 6 "42") test20
                 ]
