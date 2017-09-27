{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds #-}


-- | This module declares the representation and basic classes of references.
-- Supplies primitive functions to create references.
--
-- This module should not be imported directly.
module Control.Reference.Representation where

import Data.Proxy

-- | A reference is an accessor to a part or different view of some data.
-- The referenc has a separate getter, setter and updater. In some cases,
-- the semantics are a bit different

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

type IndexedReference i w r w' r' s t a b = i -> Reference w r w' r' s t a b

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
  :: RefMonads w r
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
type RefMonads w r = ( Functor w, Applicative w, Monad w
                     , Functor r, Applicative r, Monad r
                     )

type MU = Proxy

unusableOp :: a -> b -> MU c
unusableOp _ _ = Proxy
