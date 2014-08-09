{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- | This module declares the representation and basic classes of references.
module Control.Reference.Representation where

import Control.Applicative
import Control.Monad
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
data Reference w r s t a b
  = Reference { refGet    :: forall x . (a -> r x) -> s -> r x      
                -- ^ Getter for the lens
              , refSet    :: b -> s -> w t
                -- ^ Setter for the lens
              , refUpdate :: (a -> w b) -> s -> w t   
                -- ^ Updater for the lens. Handles monadic update functions.
              }
              
reference :: ( Functor w, Applicative w, Monad w
             , Functor r, Applicative r, Monad r ) 
          => (s -> r a) 
          -> (b -> s -> w t)
          -> ((a -> w b) -> s -> w t) 
          -> Reference w r s t a b
reference gets = Reference (\f s -> gets s >>= f)
              
-- | A monomorph 'Lens', 'Traversal', 'LensPart', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a
              
-- | The Lens is a reference that can represent an 1 to 1 relationship.
type Lens s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, Monad r )
    => Reference w r s t a b

type Lens' = Reference Identity Identity

-- | A reference that has a monad to support the empty reference and adding reference parts.
type RefPlus s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, MonadPlus r )
    => Reference w r s t a b

-- | The parital lens is a reference that can represent an 1 to 0..1 relationship.

-- TODO: partial laws
type LensPart s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, MonadPlus r, Maybe !<! r )
    => Reference w r s t a b

type LensPart' = Reference Identity Maybe

-- | The Traversal is a reference that can represent an 1 to any relationship.

-- TODO: traversal laws
type Traversal s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, MonadPlus r, [] !<! r )
    => Reference w r s t a b

type Traversal' = Reference Identity []

-- TODO: refIO laws
type RefIO s t a b
  = forall w r . ( Functor w, Applicative w, Monad w, IO !<! w
                 , Functor r, Applicative r, Monad r, IO !<! r )
    => Reference w r s t a b

-- | Strictly IO reference 
type RefIO' = Reference IO IO
    
type PartIO s t a b
  = forall w r . ( Functor w, Applicative w, Monad w, IO !<! w
                 , Functor r, Applicative r, Monad r, IO !<! r, Maybe !<! r )
    => Reference w r s t a b

-- | Strictly partial IO lens
type PartIO' = Reference IO (MaybeT IO)
    
type TravIO s t a b
  = forall w r . ( Functor w, Applicative w, Monad w, IO !<! w
                 , Functor r, Applicative r, Monad r, IO !<! r, [] !<! r )
    => Reference w r s t a b

-- | Strictly IO traversal
type TravIO' = Reference IO (ListT IO)
  
-- | States that 'm1' can be represented with 'm2'
class (m1 :: * -> *) !<! (m2 :: * -> *) where
  -- | Lifts the first monad into the second.
  liftMS :: m1 a -> m2 a

instance IO !<! (MaybeT IO) where
  liftMS = MaybeT . liftM Just

instance IO !<! (ListT IO) where
  liftMS = ListT . liftM (:[])

instance IO !<! IO where
  liftMS = id

instance Identity !<! Maybe where
  liftMS = return . runIdentity

instance Identity !<! [] where
  liftMS = return . runIdentity
  