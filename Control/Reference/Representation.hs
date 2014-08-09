{-# LANGUAGE KindSignatures, LambdaCase, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- | This module declares the representation and basic classes of references.
module Control.Reference.Representation where

import Control.Monad.Identity (Identity(..))
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.List
import Control.Monad.State
import Control.Monad.Writer

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

-- In setter and updater monads non-return values can cause unexpected behaviour. For example
-- giving back @Nothing@ inside an updater for a 'LensPart'' causes the computation to stop. 

-- TODO: represent backreferences with a type parameter (there is an (a -> s) function).
-- It is called ISO, if m = Identity, prism, if m = Maybe
-- Is there some reason to have monadic backreferences?
data Reference m s t a b
  = Reference { refGet :: s -> m a                    -- ^ Getter for the lens
              , refSet :: b -> s -> m t               -- ^ Setter for the lens
              , refUpdate :: (a -> m b) -> s -> m t   -- ^ Updater for the lens. 
                                                      -- Handles monadic update functions.
              , getClose :: s -> m ()                 -- ^ Closes the reference after a 'refGet' 
              , setClose :: s -> m ()                 -- ^ Closes the reference after a 'refSet'
              , updateClose :: s -> m ()              -- ^ Closes the reference after an update.
                                                      -- Should perform both 'getClose' and 'setClose'
              }

-- | Creates a reference with empty close operations.
reference :: Monad m => (s -> m a) -> (b -> s -> m t) -> ((a -> m b) -> s -> m t) -> Reference m s t a b
reference getter setter updater
  = Reference getter setter updater noClose noClose noClose
    where noClose = const (return ())
              
-- | A monomorph 'Lens', 'Traversal', 'LensPart', etc... 
-- Setting or updating does not change the type of the base.
type Simple t s a = t s s a a

-- | The Lens is a reference that can represent an 1 to 1 relationship.
type Lens s t a b
  = forall m . (Functor m, Applicative m, Monad m)
    => Reference m s t a b

type Lens' = Reference Identity

-- | A reference that has a monad to support the empty reference and adding reference parts.
type RefPlus s t a b
  = forall m . (Functor m, Applicative m, MonadPlus m)
    => Reference m s t a b

-- | The parital lens is a reference that can represent an 1 to 0..1 relationship.

-- TODO: partial laws
type LensPart s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadPlus m, Maybe !<! m)
    => Reference m s t a b

type LensPart' = Reference Maybe

-- | The Traversal is a reference that can represent an 1 to any relationship.

-- TODO: traversal laws
type Traversal s t a b
  = forall m . (Functor m, Applicative m, Monad m, MonadPlus m, [] !<! m)
    => Reference m s t a b

type Traversal' = Reference []

-- TODO: refIO laws
type RefIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, IO !<! m, SummarizeInto m IO)
    => Reference m s t a b

-- | Strictly IO reference 
type RefIO' = Reference IO
    
type PartIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, MaybeT IO !<! m)
    => Reference m s t a b

-- | Strictly partial IO lens
type PartIO' = Reference (MaybeT IO)
    
type TravIO s t a b
  = forall m . (Functor m, Applicative m, Monad m, ListT IO !<! m)
    => Reference m s t a b

-- | Strictly IO traversal
type TravIO' = Reference (ListT IO)
  
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

  
-- | Applies a function representing the change in some higher monad inside a lower monad.
class (mr !<! ms) => SummarizeInto ms mr where
  summarizeInto :: (a -> ms a) -> a -> mr a
  
instance SummarizeInto Maybe Identity where
  summarizeInto f s = liftM (fromMaybe s) (return (f s))

instance SummarizeInto [] Identity where
  summarizeInto f s = case f s of [] -> return s
                                  x:_ -> summarizeInto (tail . f) x

instance SummarizeInto IO IO where
  summarizeInto = id
  
instance SummarizeInto (MaybeT IO) IO where
  summarizeInto f a = liftM (fromMaybe a) (runMaybeT (f a)) 

instance SummarizeInto (ListT IO) IO where
  summarizeInto f a = runListT (f a) >>= \case [] -> return a
                                               x:_ -> summarizeInto (mapListT (liftM tail) . f) x

summarize :: SummarizeInto m Identity => (a -> m a) -> a -> a
summarize f = runIdentity . summarizeInto f


class Monad m => CloseMonad m where
  normalizeClose :: m a -> m ()

instance CloseMonad Identity where
  normalizeClose _ = return ()
  
instance CloseMonad Maybe where
  normalizeClose _ = return ()
  
instance CloseMonad [] where
  normalizeClose _ = return ()
  
instance CloseMonad IO where
  normalizeClose act = act >> return ()
  
instance CloseMonad m => CloseMonad (MaybeT m) where
  normalizeClose = MaybeT . liftM return . normalizeClose . runMaybeT
  
instance CloseMonad m => CloseMonad (ListT m) where
  normalizeClose = ListT . liftM return . normalizeClose . runListT


instance CloseMonad (StateT s IO) where
  normalizeClose = mapStateT (liftM $ \(_,s) -> ((),s))

instance CloseMonad (StateT s Identity) where
  normalizeClose = mapStateT (liftM $ \(_,s) -> ((),s))

instance Monoid s => CloseMonad (WriterT s IO) where
  normalizeClose = mapWriterT (liftM $ \(_,s) -> ((),s))

instance Monoid s => CloseMonad (WriterT s Identity) where
  normalizeClose = mapWriterT (liftM $ \(_,s) -> ((),s))
