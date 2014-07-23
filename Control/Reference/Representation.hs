{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- | This module declares the representation and basic classes of references.
module Control.Reference.Representation where

import Control.Monad.Identity (Identity(..))
import Control.Monad.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (maybeToList)

-- | A reference is an accessor to a part or different view of some data. 
-- The reference, unlike the lens has a separate getter, setter and updater.
--
-- == Reference laws
--
-- As the references are generalizations of lenses, they should conform to the lens laws:
--
-- 1) You get back what you put in:
--
-- @
-- 'lensSet' l a s >>= 'lensGet' l ≡ a
-- @
--
-- 2) Putting back what you got doesn't change anything:
--
-- @
-- 'lensGet' l a >>= \b -> 'lensSet' l b s ≡ s
-- @
--
-- 3) Setting twice is the same as setting once:
--
-- @
-- 'lensSet' l a s >>= 'lensSet' l b ≡ 'lensSet' l b s
-- @
--
-- But because they are more powerful than lenses, they should be more responsible.
-- 
-- 4) Updating something is the same as getting and then setting:
--
-- @
-- 'lensGet' l a >>= f >>= \b -> 'lensSet' l b s ≡ lensUpdate b s
-- @
--
-- == Type arguments
--   ['wm'] Writer monad, controls how the value can be reassembled when the part is changed. 
--          Usually 'Identity'.
--   ['rm'] Reader monad. Controls how part of the value can be accessed. 
--          See 'Lens', 'LensPart' and 'Traversal'
--   ['s'] The original context.
--   ['t'] The context after replacing the accessed part to something of type 'b'.
--   ['a'] The accessed part.
--   ['b'] The accessed part can be changed to this.

-- TODO: represent isomorphisms with a type parameter
-- TODO: indexed traversals
data Reference wm rm s t a b
  = Reference { lensGet :: s -> rm a                    -- ^ Getter for the lens
              , lensSet :: b -> s -> wm t               -- ^ Setter for the lens
              , lensUpdate :: (a -> wm b) -> s -> wm t  -- ^ Updater for the lens. 
                                                        -- Handles monadic update functions.
              }
              
-- | A monomorph 'Lens', 'Traversal', 'LensPart', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

-- | A monomorph 'Lens'', 'Traversal'', 'LensPart'', etc... 
-- Setting or updating does not change the type of the base.
-- Needs @LiberalTypeSynonyms@ language extension
type Simple' (w :: * -> *) t s a = t w s s a a
type SimpleRef wm rm s a = Reference wm rm s s a a
              
-- | The Lens is a reference that represents an 1 to 1 relationship.
type Lens = Reference Identity Identity
type Lens' w = Reference w Identity

-- | The Traversal is a reference that represents an 1 to any relationship.
type Traversal = Reference Identity []
type Traversal' w = Reference w []

-- | The parital lens is a reference that represents an 1 to 0..1 relationship.
type LensPart = Reference Identity Maybe
type LensPart' w = Reference w Maybe

 
-- | Combines the functionality of two monads into one. Has two functions that lift a 
-- monadic action into the result monad.
class Monad (ResultMonad m1 m2) => MonadCompose (m1 :: * -> *) (m2 :: * -> *) where
  -- | The type of the result monad
  type ResultMonad m1 m2 :: * -> *
  -- | A phantom type to help coercions. Coercions are often needed when only one of
  -- the lifting functions are used.
  data ComposePhantom m1 m2 :: *
  -- | Creates a new phantom variable to state that two liftings result in the same type.
  newComposePhantom :: ComposePhantom m1 m2
  -- | Lifts the first monad into the result monad.
  liftMC1 :: ComposePhantom m1 m2 -> m1 a -> ResultMonad m1 m2 a
  -- | Lifts the second monad into the result monad.
  liftMC2 :: ComposePhantom m1 m2 -> m2 a -> ResultMonad m1 m2 a
  
-- | States that 'm1' can be represented with 'm2'
class MonadSubsume (m1 :: * -> *) (m2 :: * -> *) where
  -- | Lifts the first monad into the second.
  liftMS :: m1 a -> m2 a
  