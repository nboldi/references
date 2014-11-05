{-# OPTIONS_HADDOCK not-home #-}

-- | An interface with references that can be used internally while generating instances
-- for 'MMorph' and tuple lens classes.
--
-- Only the public parts of "Control.Reference.Representation" are exported.
--
-- For creating a new interface with different generated elements, use this internal interface.
--
module Control.Reference.InternalInterface
       ( bireference, reference, referenceWithClose
       , module Control.Reference.Types
       , module Control.Reference.Operators
       , module Control.Reference.Combinators
       , module Control.Reference.Predefined
       , module Control.Reference.Generators
       , module Control.Reference.Predefined.Containers
       ) where

import Control.Reference.Representation
import Control.Reference.Types
import Control.Reference.Operators
import Control.Reference.Combinators
import Control.Reference.Predefined
import Control.Reference.Generators
import Control.Reference.Predefined.Containers
