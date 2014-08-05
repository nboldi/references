{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

-- | Common operators for references
module Control.Reference.UnsafeOperators where
import Control.Reference.Operators
import Control.Reference.Representation

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Data.Maybe
import Unsafe.Coerce

infixl 4 $?=
infixl 4 $*=
infixl 4 $!?=
infixl 4 $!*=

infixl 4 $?~
infixl 4 $*~
infixl 4 $!?~
infixl 4 $!*~

infixl 4 $?-
infixl 4 $*-
infixl 4 $!?-
infixl 4 $!*-


-- * Setters
          
-- | Sets the referenced data in the monad of the reference
($?=) :: LensPart' s t a b -> b -> s -> t
l $?= v = runIdentity . coerceFromF (l #= v)

($*=) :: Traversal' s t a b -> b -> s -> t
l $*= v = runIdentity . coerceFromF (l #= v)

($!?=) :: PartIO' s t a b -> b -> s -> IO t
l $!?= v = coerceFromF (l #= v)

($!*=) :: TravIO' s t a b -> b -> s -> IO t
l $!*= v = coerceFromF (l #= v)

-- * Updaters

($?~) :: LensPart' s t a b -> (a -> b) -> s -> t
l $?~ v = runIdentity . coerceFromF (l #- v)

($*~) :: Traversal' s t a b -> (a -> b) -> s -> t
l $*~ v = runIdentity . coerceFromF (l #- v)

($!?~) :: PartIO' s t a b -> (a -> IO b) -> s -> IO t
l $!?~ v = coerceFromF (l #~ liftMS . v)

($!*~) :: TravIO' s t a b -> (a -> IO b) -> s -> IO t
l $!*~ v = coerceFromF (l #~ liftMS . v)

-- * Updaters with pure function inside

($?-) :: LensPart' s t a b -> (a -> b) -> s -> t
l $?- trf = l $?~ trf

($*-) :: Traversal' s t a b -> (a -> b) -> s -> t
l $*- trf = l $*~ trf

($!?-) :: PartIO' s t a b -> (a -> b) -> s -> IO t
l $!?- trf = l $!?~ return . trf

($!*-) :: TravIO' s t a b -> (a -> b) -> s -> IO t
l $!*- trf = l $!*~ return . trf



class CoerceFrom m n | m -> n where
  coerceFrom :: a -> m b -> n b
  coerceFromF :: (a -> m b) -> a -> n b
  coerceFromF f s = coerceFrom s (f s)

instance CoerceFrom Maybe Identity where
  coerceFrom a b = return $ fromMaybe (unsafeCoerce a) b

instance CoerceFrom [] Identity where
  coerceFrom a b = return $ case b of [] -> unsafeCoerce a
                                      x:_ -> x

instance CoerceFrom (MaybeT IO) IO where
  coerceFrom a b = liftM (runIdentity . coerceFrom a) (runMaybeT b)

instance CoerceFrom (ListT IO) IO where
  coerceFrom a b = liftM (runIdentity . coerceFrom a) (runListT b)

instance MonadSubsume IO (MaybeT IO) where
  liftMS = MaybeT . liftM Just

instance MonadSubsume IO (ListT IO) where
  liftMS = ListT . liftM (:[])
