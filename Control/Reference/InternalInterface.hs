{-# OPTIONS_HADDOCK not-home #-}

-- | An interface with references that can be used internally while generating instances
-- for 'MMorph' and tuple lens classes.
--
-- Only the public parts of "Control.Reference.Representation" are exported.
--
-- For creating a new interface with different generated elements, use this internal interface.
--
module Control.Reference.InternalInterface
       ( Simple, Reference, reference, referenceWithClose
       , Lens, Partial, Traversal
       , Lens', Partial', Traversal'
       , IOLens, IOPartial, IOTraversal
       , IOLens', IOPartial', IOTraversal'
       , StateLens, StatePartial, StateTraversal
       , StateLens', StatePartial', StateTraversal'
       , WriterLens, WriterPartial, WriterTraversal
       , WriterLens', WriterPartial', WriterTraversal'
       , MMorph(..)
       , module Control.Reference.Operators
       , module Control.Reference.Predefined
       ) where

import Control.Reference.Representation
import Control.Reference.Operators
import Control.Reference.Predefined
