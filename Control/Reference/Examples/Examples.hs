{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

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
import Data.Maybe
import qualified Data.Array as Arr
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Tree as T
import System.Directory
import System.FileLock
import Network.Socket

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

test12 :: Writer [String] (Int,Int)
test12 = (both :: Simple (WriterTraversal' [String] Identity) (Int,Int) Int) 
  #| (tell . (:[]) . show) $ (0, 1)

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
name = fromLens _name

salary :: Lens Employee Employee Float Float
salary = fromLens _salary
                                     
dept = Dept (Employee "Agamemnon" 100000) [Employee "Akhilles" 30000, Employee "Menelaos" 40000]

test13 :: Writer (Sum Float) Dept
test13 = let salaryOfEmployees :: Simple (WriterTraversal' (Sum Float) Identity) Dept Float
             salaryOfEmployees = (staff&traverse &+& manager)&salary
          in salaryOfEmployees #| tell . Sum
               $ manager&name .- ("Mr. "++)
               $ dept

test14 :: [String]
test14 = traverse *- (`replicate` 'x') $ [1..10]

test15 :: (String, Char)
test15 = let lens_1 = fromLens Lens._1
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
test19 = fromJust' ?- show $ Just' (42 :: Int)
    
data Tuple a b = Tuple { _fst' :: a, _snd' :: b } deriving (Eq, Show)
         
$(makeReferences ''Tuple)

test20 :: Tuple Int String 
test20 = fst' .- length
         $ snd' .- show
         $ Tuple "almafa" 42
         
test21 :: IM.IntMap String 
test21 = element 2 ?= "two"
         $ element 3 ?- (++"_")
         $ at 4 .= Just "4"
         $ IM.fromList [(5, "5"), (2, "2")]
                  
test22 :: Seq.Seq String 
test22 = element 1 ?- ("_"++)
         $ element 3 ?= "_"
         $ Seq.fromList ["1","2","3"]
         
test23 :: Set.Set Int 
test23 = contains 2 ?= False
         $ contains 3 ?- not
         $ contains 4 ?- not
         $ Set.fromList [1,2,3]
         
test24 :: IS.IntSet 
test24 = contains 2 ?= False
         $ contains 3 ?- not
         $ contains 4 ?- not
         $ IS.fromList [1,2,3]       
         
test25 :: T.Tree Int
test25 = (\tree -> element [1,0] ?= fromJust (tree ^? element []) $ tree)
           $ element [1] ?- (+1)
           $ element [2] ?= 0
           $ T.Node 1 [T.Node 2 [], T.Node 3 [T.Node 4 []]]
           
test26 :: Arr.Array Int String
test26 = element 1 ?- (++"!")
           $ element 2 ?= "World"
           $ Arr.listArray (1,3) ["Hello","My","World"]
         

test27 :: Map.Map String Int
test27 = at "2" .= Nothing
         $ at "3" .- (fmap (subtract 1))
         $ Map.fromList [("5",5), ("3",3), ("2",2)]
         
test28 :: Int -> Maybe String
test28 = at 3 .= Nothing
         $ element 1 ?- (++"_")
         $ \a -> if a > 0 then Just (show a) else Nothing
         
example1 = 
  do result <- newEmptyMVar
     terminator <- newEmptyMVar
     forkIO $ (result ^! mvar) >>= print >> (mvar != ()) terminator >> return ()
     hello <- newMVar (Just "World")
     forkIO $ ((mvar&just&_tail&_tail) ?!- ('_':) $ hello) >> return ()
     forkIO $ ((mvar&just&(element 1)) ?!= 'u' $ hello) >> return ()
     forkIO $ ((mvar&just) ?!- ("Hello" ++) $ hello) >> return ()
     
     x <- hello ^?! (mvar & just) 
     mvar != x $ result
     terminator ^! mvar

example2 = do consoleLine != "What is your name?" $ Console
              consoleLine !- ("Hello "++) $ Console 

example3 = let logger :: String -> Simple IOLens a a
               logger n = referenceWithClose
                            return (const (morph $ putStrLn $ n ++ ": read done"))
                            (\b _ -> return b) (const (morph $ putStrLn $ n ++ ": write done"))
                            (\trf a -> trf a) (const (morph $ putStrLn $ n ++ ": update done"))
               loggedConsole = logger "a" & logger "b" & consoleLine
           in do loggedConsole != "Enter 'x'" $ Console
                 x <- read <$> (Console ^! loggedConsole) :: IO Int
                 loggedConsole != "Enter 'y'" $ Console
                 loggedConsole !- (("The result is: " ++) . show . (x +) . read) $ Console

-- | Reference to the contents of the file. Currently not thread-safe
fileRef :: Simple IOPartial FilePath String
fileRef = reference (\fp -> do exist <- morph (doesFileExist fp) 
                               if exist then morph (withFileLock fp Shared 
                                                       $ const $ readFile fp)
                                        else morph Nothing)
                    (\str fp -> morph (withFileLock fp Exclusive 
                                          $ const $ writeFile fp str) >> return fp) 
                    (\trf fp -> do exist <- morph (doesFileExist fp)
                                   when exist (do lock <- morph (lockFile fp Exclusive) 
                                                  morph (readFile fp) >>= trf 
                                                     >>= morph . writeFile fp
                                                  morph (unlockFile lock))
                                   return fp)


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
                 , TestCase $ assertEqual "test12" ["0","1"] (execWriter test12)
                 , TestCase $ assertEqual "test13" (dept { _manager = Employee "Mr. Agamemnon" 100000 }, Sum 170000)
                                                   (runWriter test13)
                 , TestCase $ assertEqual "test14" [replicate i 'x' | i <- [1..10]] test14
                 , TestCase $ assertEqual "test15" ("2",'a') test15
                 , TestCase $ assertEqual "test16" (Left 3, Right 1) test16
                 , TestCase $ assertEqual "test17" (PWrapped (Just "3")) test17
                 , TestCase $ assertEqual "test18" (MWrapped (Just "3")) test18
                 , TestCase $ assertEqual "test19" (Just' "42") test19
                 , TestCase $ assertEqual "test20" (Tuple 6 "42") test20
                 , TestCase $ assertEqual "test21" (IM.fromList [(2,"two"),(4,"4"),(5,"5")]) test21
                 , TestCase $ assertEqual "test22" (Seq.fromList ["1","_2","3"]) test22
                 , TestCase $ assertEqual "test23" (Set.fromList [1,4]) test23
                 , TestCase $ assertEqual "test24" (IS.fromList [1,4]) test24
                 , TestCase $ assertEqual "test25" (T.Node 1 [T.Node 2 [], T.Node 4 [T.Node 1 []]]) test25
                 , TestCase $ assertEqual "test26" (Arr.listArray (1,3) ["Hello!","World","World"]) test26
                 , TestCase $ assertEqual "test27" (Map.fromList [("5",5), ("3",2)]) test27
                 , TestCase $ assertEqual "test28" ["1_", "2"] (catMaybes $ map test28 [0..3])
                 ]
