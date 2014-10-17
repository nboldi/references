{-# LANGUAGE TypeFamilies #-}
module Control.Reference.Predefined.Containers.Tree where

import Control.Reference.InternalInterface
import Control.Reference.TupleInstances

import qualified Data.Tree as Tree

instance Association (Tree.Tree v) where
  type AssocIndex (Tree.Tree v) = [Int]
  type AssocElem (Tree.Tree v) = v
  element is = simplePartial (accessNode is)
    where accessNode [] (Tree.Node lab for) 
            = Just (lab, \lab' -> Tree.Node lab' for)
          accessNode (i:is) (Tree.Node lab for)
            = case for ^? element i of
                Just subFor -> just&_2 .- (\upd -> Tree.Node lab . (\v -> element i .= v $ for) . upd)
                                $ accessNode is subFor
                Nothing -> Nothing