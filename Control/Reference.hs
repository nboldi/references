-- | A frontend module for the Control.Reference package

module Control.Reference
( module Control.Reference.InternalInterface
, module Control.Reference.Predefined.Containers.Tree
, module Control.Reference.TH.Records
, module Control.Reference.TupleInstances
) where

import Control.Reference.InternalInterface
import Control.Reference.Predefined.Containers.Tree()

-- generator modules
import Control.Reference.TH.Records

-- generated classes and instances
import Control.Reference.TupleInstances
