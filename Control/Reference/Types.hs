{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the polymorphic types of the created references.
-- The actual type of a reference can be different for every usage,
-- the polymorphic type gives a lower bound on the actual one.
module Control.Reference.Types where
    
import Control.Reference.Representation
    
import Control.Instances.Morph
import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.ST (ST)
import Data.Proxy
    
instance Alternative MU where
  empty = Proxy
  _ <|> _ = Proxy
  
instance MonadPlus MU where
  mzero = Proxy
  mplus _ _ = Proxy
         
-- | A monomorph 'Lens', 'Traversal', 'Partial', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

-- A read-only reference
type Getter r s t a b = Reference MU r MU MU s t a b

-- A write (and update) -only reference
type Setter w s t a b = Reference w MU MU MU s t a b

-- * Pure references
                 
-- | A two-way 'Reference' that represents an isomorphism between two datatypes.
-- Can be used to access the same data in two different representations.
type Iso s t a b
  = forall w r w' r' . (RefMonads w r, RefMonads w' r') => Reference w r w' r' s t a b
         
-- | A partial lens that can be turned to get a total lens.         
type Prism s t a b
  = forall w r w' r' . (RefMonads w r, RefMonads w' r'
                       , MonadPlus r, Morph Maybe r 
                       , MonadPlus w', Morph Maybe w') 
      => Reference w r w' r' s t a b
                 
-- | A 'Reference' that can access a part of data that exists in the context.
-- A 'Lens' can have any read and write semantics that a 'Reference' can have.
type Lens s t a b
  = forall w r . RefMonads w r => Reference w r MU MU s t a b

-- | A reference that may not have the accessed element, and that can
-- look for the accessed element in multiple locations.
type RefPlus s t a b
  = forall w r . ( RefMonads w r, MonadPlus r )
    => Reference w r MU MU s t a b
    
-- | Partial lens. A 'Reference' that can access data that may not exist in the context.
-- Every lens is a partial lens.
--
-- Any reference that is a partial lens should only perform the action given to its
-- 'updateRef' function if it can get a value (the value returned by 'getRef' is not
-- the lifted form of 'Nothing').
type Partial s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, MonadPlus r, Morph Maybe r )
    => Reference w r MU MU s t a b
    
-- | A reference that can access data that is available in a number of instances
-- inside the contexts.
-- 
-- Any reference that is a 'Traversal' should perform the action given to its
-- updater in the exactly the same number of times that is the number of the values
-- returned by it's 'getRef' function.
type Traversal s t a b
  = forall w r . (RefMonads w r, MonadPlus r, Morph Maybe r, Morph [] r )
    => Reference w r MU MU s t a b

-- * References for 'IO'

class ( Morph IO w, Morph IO r
      , MorphControl IO w, MorphControl IO r ) => IOMonads w r where
instance ( Morph IO w, Morph IO r
         , MorphControl IO w, MorphControl IO r ) => IOMonads w r where

-- | A reference that can access mutable data.
type IOLens s t a b
  = forall w r . ( RefMonads w r, IOMonads w r )
    => Reference w r MU MU s t a b

-- | A reference that can access mutable data that may not exist in the context.
type IOPartial s t a b
  = forall w r . (RefMonads w r, IOMonads w r, MonadPlus r, Morph Maybe r )
    => Reference w r MU MU s t a b

type IOTraversal s t a b
  = forall w r . ( RefMonads w r, IOMonads w r, MonadPlus r, Morph Maybe r, Morph [] r )
    => Reference w r MU MU s t a b

-- * References for 'StateT'

-- | A reference that can access a value inside a 'StateT' transformed monad.
type StateLens st m s t a b
  = forall w r . ( RefMonads w r, Morph (StateT st m) w, Morph (StateT st m) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside a 'StateT' transformed monad
-- that may not exist.
type StatePartial st m s t a b
  = forall w r . ( RefMonads w r, Morph (StateT st m) w, MonadPlus r, Morph Maybe r, Morph (StateT st m) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside a 'StateT' transformed monad
-- that may exist in multiple instances.
type StateTraversal st m s t a b
  = forall w r . ( RefMonads w r, Morph (StateT st m) w, MonadPlus r, Morph Maybe r, Morph [] r, Morph (StateT st m) r )
    => Reference w r MU MU s t a b

-- * References for 'WriterT'

-- | A reference that can access a value inside a 'WriterT' transformed monad.
type WriterLens st m s t a b
  = forall w r . ( RefMonads w r, Morph (WriterT st m) w, Morph (WriterT st m) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside a 'WriterT' transformed monad
-- that may not exist.
type WriterPartial st m s t a b
  = forall w r . ( RefMonads w r, Morph (WriterT st m) w, MonadPlus r, Morph Maybe r, Morph (WriterT st m) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside a 'WriteT' transformed monad
-- that may exist in multiple instances.
type WriterTraversal st m s t a b
  = forall w r . ( RefMonads w r, Morph (WriterT st m) w, MonadPlus r, Morph Maybe r, Morph [] r, Morph (WriterT st m) r )
    => Reference w r MU MU s t a b

-- * References for 'ST'

-- | A reference that can access a value inside an 'ST' transformed monad.
type STLens st s t a b
  = forall w r . ( RefMonads w r, Morph (ST st) w, Morph (ST st) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside an 'ST' transformed monad
-- that may not exist.
type STPartial st s t a b
  = forall w r . ( RefMonads w r, Morph (ST st) w, MonadPlus r, Morph Maybe r, Morph (ST st) r )
    => Reference w r MU MU s t a b

-- | A reference that can access a value inside an 'ST' transformed monad
-- that may exist in multiple instances.
type STTraversal st s t a b
  = forall w r . ( RefMonads w r, Morph (ST st) w, MonadPlus r, Morph Maybe r, Morph [] r, Morph (ST st) r )
    => Reference w r MU MU s t a b

-- | A class for representing calculation in a simpler monad.
-- 
-- @pullBack . sink === id@
class MorphControl (m1 :: * -> *) (m2 :: * -> *) where
  data MSt m1 m2 a :: *
  sink :: m2 a -> m1 (MSt m1 m2 a)
  pullBack :: m1 (MSt m1 m2 a) -> m2 a
  
instance Monad m => MorphControl m (MaybeT m) where
  newtype MSt m (MaybeT m) a = MaybeMSt { fromMaybeMSt :: Maybe a }
  sink = liftM MaybeMSt . runMaybeT
  pullBack = MaybeT . liftM fromMaybeMSt
  
instance Monad m => MorphControl m (ListT m) where
  newtype MSt m (ListT m) a = ListMSt { fromListMSt :: [a] }
  sink = liftM ListMSt . runListT
  pullBack = ListT . liftM fromListMSt
  
-- FIXME: conflicts with other instance declarations
-- instance (Monad m, Morph m m) => MorphControl m m where
  -- newtype MSt m m a = ReflMSt { fromReflMSt :: a }
  -- sink = liftM ReflMSt
  -- pullBack = liftM fromReflMSt
  
instance MorphControl IO IO where
  newtype MSt IO IO a = ReflIOMSt { fromReflIOMSt :: a }
  sink = liftM ReflIOMSt
  pullBack = liftM fromReflIOMSt
    
instance MorphControl Identity m where
  newtype MSt Identity m a = IdMSt { fromIdMSt :: m a }
  sink = Identity . IdMSt
  pullBack = fromIdMSt . runIdentity
  
instance (Monad m) => MorphControl m MU where
  newtype MSt m MU a = ProxyMSt ()
  sink _ = return (ProxyMSt ())
  pullBack _ = Proxy
  
