-- | A frontend module for the Control.Reference package

module Control.Reference 
( Reference(Reference), Lens, Lens', Traversal, Traversal', LensPart, LensPart'
, module Control.Reference.Operators
, module Control.Reference.Predefined
, module Control.Reference.TH.Monad
, module Control.Reference.TH.Generate
, module Control.Reference.TupleInstances
) where

import Control.Reference.Representation
import Control.Reference.Operators
import Control.Reference.Predefined
import Control.Reference.TH.Monad
import Control.Reference.TH.Generate

-- generated classes and instances
import Control.Reference.TH.MonadInstances
import Control.Reference.TupleInstances
