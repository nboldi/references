{-# OPTIONS_HADDOCK not-home #-}

-- | An interface with references that can be used internally while generating instances
-- for 'MMorph' and tuple lens classes.
--
-- Only the public parts of "Control.Reference.Representation" are exported.
--
-- For creating a new interface with different generated elements, use this internal interface.
--
module Control.Reference.InternalInterface
       ( Simple, Reference, bireference, reference, referenceWithClose
       , Iso
       , Lens, Partial, Traversal
       , IOLens, IOPartial, IOTraversal
       , StateLens, StatePartial, StateTraversal
       , WriterLens, WriterPartial, WriterTraversal
       , MMorph(..)
       , module Control.Reference.Operators
       , module Control.Reference.Predefined
       , module Control.Reference.Predefined.Containers
       ) where

import Control.Reference.Representation
import Control.Reference.Operators
import Control.Reference.Predefined
import Control.Reference.Predefined.Containers
