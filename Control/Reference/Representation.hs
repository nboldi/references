{-# LANGUAGE KindSignatures, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- | This module declares the representation and basic classes of references.
module Control.Reference.Representation where

import Control.Monad.Identity (Identity(..))
import Control.Applicative(Applicative)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.List (ListT(..))

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
--   ['m'] The monadic context that controls how the value can be accessed.
--   ['s'] The original context.
--   ['t'] The context after replacing the accessed part to something of type 'b'.
--   ['a'] The accessed part.
--   ['b'] The accessed part can be changed to this.

-- TODO: represent isomorphisms with a type parameter
-- TODO: indexed traversals
data Reference m s t a b
  = Reference { lensGet :: s -> m a                   -- ^ Getter for the lens
              , lensSet :: b -> s -> m t              -- ^ Setter for the lens
              , lensUpdate :: (a -> m b) -> s -> m t  -- ^ Updater for the lens. 
                                                      -- Handles monadic update functions.
              }
              
-- | A monomorph 'Lens', 'Traversal', 'LensPart', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

-- | A monomorph 'Lens'', 'Traversal'', 'LensPart'', etc... 
-- Setting or updating does not change the type of the base.
-- Needs @LiberalTypeSynonyms@ language extension
type Simple' (w :: * -> *) t s a = t w s s a a
type SimpleRef m s a = Reference m s s a a
              
-- | The Lens is a reference that can represent an 1 to 1 relationship.
type Lens s t a b
  = forall m . (Functor m, Applicative m, Monad m)
    => Reference m s t a b

type Lens' = Reference Identity


-- | The parital lens is a reference that can represent an 1 to 0..1 relationship.
type LensPart s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadSubsume Maybe m)
    => Reference m s t a b

type LensPart' = Reference Maybe

-- | The Traversal is a reference that can represent an 1 to any relationship.
type Traversal s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadSubsume [] m)
    => Reference m s t a b

type Traversal' = Reference []

type RefIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadSubsume IO m, SummarizeFor m IO)
    => Reference m s t a b

type RefIO' = Reference IO
    
type PartIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadSubsume (MaybeT IO) m)
    => Reference m s t a b

type PartIO' = Reference (MaybeT IO)
    
type TravIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadSubsume (ListT IO) m)
    => Reference m s t a b

type TravIO' = Reference (ListT IO)
                        
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
  
class Summarize (m :: * -> *) where
  
  -- | Applies the function to the original value, producing a pure value.
  summarize :: (a -> m a) -> a -> a
  summarize f s = runIdentity $ summarizeM (return . f) s

  summarizeM :: (Monad f) => (a -> f (m a)) -> a -> f a

instance Summarize Identity where
  summarizeM f = liftM runIdentity . f
  
instance Summarize Maybe where
  summarizeM f s = liftM (fromMaybe s) (f s)

instance Summarize [] where
  summarizeM f s = f s >>= \case [] -> return s
                                 x:_ -> summarizeM (liftM tail . f) x

class SummarizeFor ms mr where
  summarizeFor :: (a -> ms a) -> a -> mr a

instance SummarizeFor IO IO where
  summarizeFor f = f
  
instance SummarizeFor (MaybeT IO) IO where
  summarizeFor f = summarizeM (runMaybeT . f)

instance SummarizeFor (ListT IO) IO where
  summarizeFor f = summarizeM (runListT . f)
