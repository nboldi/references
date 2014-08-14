{- LANGUAGE CPP -}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}


-- | This module declares the representation and basic classes of references.
--
-- This module should not be imported directly.

-- TODO: references that can be flipped (isomorphisms and prisms)
-- TODO: indexed traversals
-- TODO: read-only and write-only references
module Control.Reference.Representation where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity(..))
import Control.Monad.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

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
--   ['s'] The type of the original context.
--   ['t'] The after replacing the accessed part to something of type 'b'
--          the type of the context changes to 't'.
--   ['a'] The type of the accessed part.
--   ['b'] The accessed part can be changed to something of this type.
--
-- Usually 's' and 'b' determines 't', 't' and 'a' determines 's'.
--
-- The reader monad usually have more information (@MMorph 'w' 'r'@).
--

data Reference w r s t a b
  = Reference { refGet    :: forall x . (a -> r x) -> s -> r x      
                -- ^ Getter for the lens. Takes a monadic function and runs it
                -- on the accessed value. This is necessary to run actions after
                -- a read.
              , refSet    :: b -> s -> w t
                -- ^ Setter for the lens
              , refUpdate :: (a -> w b) -> s -> w t   
                -- ^ Updater for the lens. Handles monadic update functions.
              }

-- | Creates a reference.
reference :: ( Functor w, Applicative w, Monad w
             , Functor r, Applicative r, Monad r ) 
          => (s -> r a) -- ^ Getter
          -> (b -> s -> w t) -- ^ Setter
          -> ((a -> w b) -> s -> w t) -- ^ Updater
          -> Reference w r s t a b
reference gets = Reference (\f s -> gets s >>= f)

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
  -> Reference w r s t a b
referenceWithClose get getClose set setClose update updateClose
  = Reference (\f s -> (get s >>= f) <* getClose s)
              (\b s -> set b s <* setClose s)
              (\trf s -> update trf s <* updateClose s)

-- | A simple class to enforce that both reader and writer semantics of the reference are 'Monad's
-- (as well as 'Applicative's and 'Functor's)
class ( Functor w, Applicative w, Monad w
      , Functor r, Applicative r, Monad r
      ) => RefMonads w r where
instance ( Functor w, Applicative w, Monad w
         , Functor r, Applicative r, Monad r )
         => RefMonads w r where

-- | A monomorph 'Lens', 'Traversal', 'Partial', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

-- * Pure references
                    
-- | A 'Reference' that can access a part of data that exists in the context.
-- Every well-formed 'Reference' is a 'Lens'.
type Lens s t a b
  = forall w r . RefMonads w r => Reference w r s t a b

-- | Strict lens. A 'Reference' that must access a part of data that surely exists
-- in the context.
type Lens' = Reference Identity Identity

-- | A reference that may not have the accessed element, and that can
-- look for the accessed element in multiple locations.
type RefPlus s t a b
  = forall w r . ( RefMonads w r, MonadPlus r )
    => Reference w r s t a b

-- | Partial lens. A 'Reference' that can access data that may not exist in the context.
-- Every lens is a partial lens.
--
-- Any reference that is a partial lens should only perform the action given to its
-- 'updateRef' function if it can get a value (the value returned by 'getRef' is not
-- the lifted form of 'Nothing').
type Partial s t a b
  = forall w r . ( Functor w, Applicative w, Monad w
                 , Functor r, Applicative r, MonadPlus r, MMorph Maybe r )
    => Reference w r s t a b

-- | Strict partial lens. A 'Reference' that must access data that may not exist
-- in the context.
type Partial' = Reference Identity Maybe

-- | A reference that can access data that is available in a number of instances
-- inside the contexts.
-- 
-- Any reference that is a 'Traversal' should perform the action given to its
-- updater in the exactly the same number of times that is the number of the values
-- returned by it's 'getRef' function.
type Traversal s t a b
  = forall w r . (RefMonads w r, MonadPlus r, MMorph [] r )
    => Reference w r s t a b

-- | Strict traversal. A reference that must access data that is available in a
-- number of instances inside the context.
type Traversal' = Reference Identity []

-- * References for 'IO'

-- | A reference that can access mutable data.
type IOLens s t a b
  = forall w r . ( RefMonads w r, MMorph IO w, MMorph IO r )
    => Reference w r s t a b

-- | A reference that must access mutable data that is available in the context.
type IOLens' = Reference IO IO

-- | A reference that can access mutable data that may not exist in the context.
type IOPartial s t a b
  = forall w r . (RefMonads w r, MMorph IO w, MonadPlus r, MMorph IO r, MMorph Maybe r )
    => Reference w r s t a b

-- | A reference that must access mutable data that may not exist in the context.
type IOPartial' = Reference IO (MaybeT IO)
    
type IOTraversal s t a b
  = forall w r . ( RefMonads w r, MMorph IO w, MonadPlus r, MMorph IO r, MMorph [] r )
    => Reference w r s t a b

-- | A reference that can access mutable data that is available in a number of
-- instances inside the contexts.
type IOTraversal' = Reference IO (ListT IO)

-- * References for 'StateT'

-- | A reference that can access a value inside a 'StateT' transformed monad.
type StateLens st m s t a b
  = forall w r . ( RefMonads w r, MMorph (StateT st m) w, MMorph (StateT st m) r )
    => Reference w r s t a b

-- | A reference that must access a value inside a 'StateT' transformed monad.
type StateLens' s m = Reference (StateT s m) (StateT s m)

-- | A reference that can access a value inside a 'StateT' transformed monad
-- that may not exist.
type StatePartial st m s t a b
  = forall w r . ( RefMonads w r, MMorph (StateT st m) w, MonadPlus r, MMorph Maybe r, MMorph (StateT st m) r )
    => Reference w r s t a b

-- | A reference that must access a value inside a 'StateT' transformed monad
-- that may not exist.
type StatePartial' s m = Reference (StateT s m) (MaybeT (StateT s m))

-- | A reference that can access a value inside a 'StateT' transformed monad
-- that may exist in multiple instances.
type StateTraversal st m s t a b
  = forall w r . ( RefMonads w r, MMorph (StateT st m) w, MonadPlus r, MMorph [] r, MMorph (StateT st m) r )
    => Reference w r s t a b

-- | A reference that must access a value inside a 'StateT' transformed monad
-- that may exist in multiple instances.
type StateTraversal' s m = Reference (StateT s m) (ListT (StateT s m))

-- * References for 'WriterT'

-- | A reference that can access a value inside a 'WriterT' transformed monad.
type WriterLens st m s t a b
  = forall w r . ( RefMonads w r, MMorph (WriterT st m) w, MMorph (WriterT st m) r )
    => Reference w r s t a b

-- | A reference that must access a value inside a 'WriterT' transformed monad.
type WriterLens' s m = Reference (WriterT s m) (WriterT s m)

-- | A reference that can access a value inside a 'WriterT' transformed monad
-- that may not exist.
type WriterPartial st m s t a b
  = forall w r . ( RefMonads w r, MMorph (WriterT st m) w, MonadPlus r, MMorph Maybe r, MMorph (WriterT st m) r )
    => Reference w r s t a b

-- | A reference that must access a value inside a 'WriteT' transformed monad
-- that may not exist.
type WriterPartial' s m = Reference (WriterT s m) (MaybeT (WriterT s m))

-- | A reference that can access a value inside a 'WriteT' transformed monad
-- that may exist in multiple instances.
type WriterTraversal st m s t a b
  = forall w r . ( RefMonads w r, MMorph (WriterT st m) w, MonadPlus r, MMorph [] r, MMorph (WriterT st m) r )
    => Reference w r s t a b
    
-- | A reference that must access a value inside a 'WriteT' transformed monad
-- that may exist in multiple instances.
type WriterTraversal' s m = Reference (WriterT s m) (ListT (WriterT s m))
              
-- | States that 'm1' can be represented with 'm2'.
-- That is because 'm2' contains more infromation than 'm1'.
--
-- The 'MMorph' relation defines a natural transformation from 'm1' to 'm2'
-- that keeps the following laws:
--
-- > morph (return x)  =  return x
-- > morph (m >>= f)   =  morph m >>= morph . f
-- 
-- It is a reflexive and transitive relation.
--
class MMorph (m1 :: * -> *) (m2 :: * -> *) where
  -- | Lifts the first monad into the second.
  morph :: m1 a -> m2 a

instance MMorph IO (MaybeT IO) where
  morph = MaybeT . liftM Just

instance MMorph IO (ListT IO) where
  morph = ListT . liftM (:[])

instance MMorph IO IO where
  morph = id

instance MMorph Identity Maybe where
  morph = return . runIdentity

instance MMorph Identity [] where
  morph = return . runIdentity
  
