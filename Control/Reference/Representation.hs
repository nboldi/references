{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances
           , MultiParamTypeClasses, TypeFamilies #-}


-- | This module declares the representation and basic classes of references.
--
-- This module should not be imported directly.
module Control.Reference.Representation where

import Data.Proxy
import Control.Instances.Morph
import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.ST (ST)

-- | A reference is an accessor to a part or different view of some data. 
-- The referenc has a separate getter, setter and updater. In some cases,
-- the semantics are a bit different
--
-- == Reference laws
--
-- As the references are generalizations of lenses, they should conform to the lens laws:
--
-- 1) You get back what you put in:
--
-- @
-- 'refSet' l a s >>= 'refGet' l return ≡ a
-- @
--
-- 2) Putting back what you got doesn't change anything:
--
-- @
-- 'refGet' l return a >>= \\b -> 'refSet' l b s ≡ s
-- @
--
-- 3) Setting twice is the same as setting once:
--
-- @
-- 'refSet' l a s >>= 'refSet' l b ≡ 'refSet' l b s
-- @
--
-- But because update, set and get are different operations, .
-- 
-- 4) Updating something is the same as getting and then setting (if the reader and writer monads are the same, or one can be converted into the other):
--
-- @
-- 'refGet' l a >>= f >>= \\b -> 'refSet' l b s ≡ 'refUpdate' l f s
-- @
--
-- This has some consequences. For example @lensUpdate l id = return@.
--
-- == Type arguments of 'Reference'
--   ['w'] Writer monad, controls how the value can be reassembled when the part is changed.
--          See differences between 'Lens', 'IOLens' and 'StateLens'
--   ['r'] Reader monad. Controls how part of the value can be asked. 
--          See differences between 'Lens', 'Partial' and 'Traversal'
--   [@w'@] Backward writer monad. See 'turn'
--   [@r'@] Backward reader monad. See 'turn'
--   ['s'] The type of the original context.
--   ['t'] The after replacing the accessed part to something of type 'b'
--          the type of the context changes to 't'.
--   ['a'] The type of the accessed part.
--   ['b'] The accessed part can be changed to something of this type.
--
-- Usually 's' and 'b' determines 't', 't' and 'a' determines 's'.
--
-- The reader monad usually have more information (@Morph 'w' 'r'@).
--

data Reference w r w' r' s t a b
  = Reference { refGet    :: forall x . (a -> r x) -> s -> r x      
                -- ^ Getter for the lens. Takes a monadic function and runs it
                -- on the accessed value. This is necessary to run actions after
                -- a read.
              , refSet    :: b -> s -> w t
                -- ^ Setter for the lens
              , refUpdate :: (a -> w b) -> s -> w t   
                -- ^ Updater for the lens. Handles monadic update functions.
              , refGet'     :: forall x . (s -> r' x) -> a -> r' x
              , refSet'     :: t -> a -> w' b
              , refUpdate'  :: (s -> w' t) -> a -> w' b
              }

-- Creates a two-way reference
bireference :: (RefMonads w r, RefMonads w' r')
            => (s -> r a) -- ^ Getter
            -> (b -> s -> w t) -- ^ Setter
            -> ((a -> w b) -> s -> w t) -- ^ Updater
            -> (a -> r' s) -- ^ Backward getter
            -> (t -> a -> w' b) -- ^ Backward setter
            -> ((s -> w' t) -> a -> w' b) -- ^ Backward updater
            -> Reference w r w' r' s t a b
bireference get set upd get' set' upd'
  = Reference (\f s -> get s >>= f) set upd 
              (\f s -> get' s >>= f) set' upd'
  
-- | Creates a reference.
reference :: ( RefMonads w r ) 
          => (s -> r a) -- ^ Getter
          -> (b -> s -> w t) -- ^ Setter
          -> ((a -> w b) -> s -> w t) -- ^ Updater
          -> Reference w r MU MU s t a b
reference gets sets updates = Reference (\f s -> gets s >>= f) sets updates 
                                        unusableOp unusableOp unusableOp

-- | Creates a reference where all operations are added in their original form.
--
-- The use of this method is not suggested, because it is closely related to the
-- representation of the references.
rawReference :: (RefMonads w r, RefMonads w' r')
             => (forall x . (a -> r x) -> s -> r x)     -- ^ Getter
             -> (b -> s -> w t)                         -- ^ Setter
             -> ((a -> w b) -> s -> w t)                -- ^ Updater
             -> (forall x . (s -> r' x) -> a -> r' x)    -- ^ Backward getter
             -> (t -> a -> w' b)                        -- ^ Backward setter
             -> ((s -> w' t) -> a -> w' b)              -- ^ Backward updater
             -> Reference w r w' r' s t a b
rawReference = Reference
                                        
-- | Creates a reference with explicit close operations that are executed
-- after the data is accessed.
referenceWithClose
  :: ( Functor w, Applicative w, Monad w
             , Functor r, Applicative r, Monad r ) 
  => (s -> r a) -- ^ Getter
     -> (s -> r ()) -- ^ Close after getting
  -> (b -> s -> w t) -- ^ Setter
     -> (s -> w ()) -- ^ Close after setting
  -> ((a -> w b) -> s -> w t) -- ^ Updater
     -> (s -> w ()) -- ^ Close after updating
  -> Reference w r MU MU s t a b
referenceWithClose get getClose set setClose update updateClose
  = Reference (\f s -> (get s >>= f) <* getClose s)
              (\b s -> set b s <* setClose s)
              (\trf s -> update trf s <* updateClose s)
              unusableOp unusableOp unusableOp
                
-- | A simple class to enforce that both reader and writer semantics of the reference are 'Monad's
-- (as well as 'Applicative's and 'Functor's)
class ( Functor w, Applicative w, Monad w
      , Functor r, Applicative r, Monad r
      ) => RefMonads w r where
instance ( Functor w, Applicative w, Monad w
         , Functor r, Applicative r, Monad r )
         => RefMonads w r where

type MU = Proxy

instance Alternative MU where
  empty = Proxy
  _ <|> _ = Proxy
  
instance MonadPlus MU where
  mzero = Proxy
  mplus _ _ = Proxy

unusableOp :: a -> b -> MU c
unusableOp _ _ = Proxy
         
-- | A monomorph 'Lens', 'Traversal', 'Partial', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

type Getter r s a = Reference MU r MU MU s s a a

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
-- Every well-formed 'Reference' is a 'Lens'.
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

class MorphControl (m1 :: * -> *) (m2 :: * -> *) where
  type MSt m1 m2 :: * -> *
  sink :: m2 a -> m1 (MSt m1 m2 a)
  pullBack :: m1 (MSt m1 m2 a) -> m2 a
  
instance MorphControl IO (MaybeT IO) where
  type MSt IO (MaybeT IO) = Maybe
  sink (MaybeT m) = m
  pullBack = MaybeT
  
-- FIXME: conflicts with MorphControl m MU
-- instance (Monad m, Morph m m) => MorphControl m m where
  -- type MSt m m = Identity
  -- sink m = m >>= return . Identity
  -- pullBack m = m >>= return . runIdentity
  
instance (Monad IO, Morph IO IO) => MorphControl IO IO where
  type MSt IO IO = Identity
  sink m = m >>= return . Identity
  pullBack m = m >>= return . runIdentity
  
instance (Monad m, Morph m MU) => MorphControl m MU where
  type MSt m MU = Proxy
  sink _ = return Proxy
  pullBack _ = Proxy
  
