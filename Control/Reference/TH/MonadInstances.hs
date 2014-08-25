{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- | A basic set of instances derived using "Control.Reference.TH.Monad".
-- 
-- == Structure defined
--
-- @
--            'ListT' 'IO'
--           /         \\
--         []       'Control.Monad.Trans.Maybe.MaybeT' 'IO'
--         |      /     |
--       'Maybe'         'IO'
--            \\       /
--            'Control.Monad.Trans.Identity.Identity'
-- @
module Control.Reference.TH.MonadInstances () where

import Control.Reference.InternalInterface
import Control.Reference.TH.Monad

import Control.Monad.Identity
import Control.Monad.Trans.Maybe as Trans
import Control.Monad.Trans.List as Trans
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Monoid (Monoid)
import Data.Maybe
import Language.Haskell.TH as TH

$(makeMonadRepr ''Identity          ''Maybe                     [e| return . runIdentity |])
$(makeMonadRepr ''Identity          ''IO                        [e| return . runIdentity |])
$(makeMonadRepr ''Maybe             [t| MaybeT IO |]            [e| MaybeT . return |])
$(makeMonadRepr ''IO                [t| MaybeT IO |]            [e| MaybeT . liftM Just |])
$(makeMonadRepr ''Maybe             TH.ListT                    [e| maybeToList |])
$(makeMonadRepr TH.ListT            [t| Trans.ListT IO |]       [e| Trans.ListT . return |])
$(makeMonadRepr ''IO                [t| Trans.ListT IO |]       [e| Trans.ListT . liftM (:[]) |])
$(makeMonadRepr [t| MaybeT IO |]    [t| Trans.ListT IO |]       [e| Trans.ListT . liftM maybeToList . runMaybeT |])

instance Monad m => MMorph [] (ListT (StateT s m)) where
  morph = Trans.ListT . return
  
instance Monad m => MMorph Maybe (ListT (StateT s m)) where
  morph = (morph :: [a] -> ListT (StateT s m) a) . morph

instance (Monad m, Monoid s) => MMorph [] (Trans.ListT (WriterT s m)) where
  morph = Trans.ListT . return
  
instance (Monad m, Monoid s) => MMorph Maybe (ListT (WriterT s m)) where
  morph = (morph :: [a] -> ListT (WriterT s m) a) . morph

instance Monad m => MMorph (StateT s m) (ListT (StateT s m)) where
  morph = lift