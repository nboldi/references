module Main where

import System.Exit
import Test.HUnit
import Control.Instances.Morph
import Control.Monad.Trans.Maybe
import qualified Control.Lens as Lens
import Control.Concurrent
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Array as Arr
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Tree as T

import Control.Reference.Examples.Examples

main = do Counts _ _ err fail <- runTestTT tests
          if (err + fail == 0) then exitSuccess
                               else exitFailure
        
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

