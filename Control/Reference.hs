-- | A frontend module for the Control.Reference package

module Control.Reference
( module Control.Reference.InternalInterface
, module Control.Reference.Predefined.Containers.Tree
, module Control.Reference.TH.Monad
, module Control.Reference.TH.Records
, module Control.Reference.TH.MonadInstances
, module Control.Reference.TupleInstances
) where

import Control.Reference.InternalInterface
import Control.Reference.Predefined.Containers.Tree

-- generator modules
import Control.Reference.TH.Monad
import Control.Reference.TH.Records

-- generated classes and instances
import Control.Reference.TH.MonadInstances
import Control.Reference.TupleInstances
