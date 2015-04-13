module Main where

import Control.Reference.Examples.Examples

import System.Exit
import Test.HUnit

main = do Counts _ _ err fail <- runTestTT tests
          if (err + fail == 0) then exitSuccess
                               else exitFailure

