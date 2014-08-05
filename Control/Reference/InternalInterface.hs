module Control.Reference.InternalInterface
       ( Reference(Reference)
       , Lens, Lens'
       , Traversal, Traversal'
       , LensPart, LensPart'
       , MonadSubsume(..)
       , module ExportedModules
       ) where

import Control.Reference.Representation
import Control.Reference.Operators as ExportedModules
import Control.Reference.UnsafeOperators as ExportedModules
import Control.Reference.Predefined as ExportedModules
