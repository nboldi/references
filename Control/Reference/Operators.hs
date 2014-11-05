{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}

--
-- | Common operators for using references.
--
-- There are four kinds of operator for every type of reference.
-- The operators are either getters ('^.' and '^?'), setters ('.=' and '!='), 
-- monadic updaters ('.~' and '!~'), pure updaters ('.-' and '!-') or action performers (@!|@).
--
-- The former operators (with the dot) are pure operators, the later are monadic operators. For example, @(1,2) ^. _1@ results in a pure numeric value, while @Right 4 ^? right@ produces @Just 4@ (or a higher level value representing that).
--

module Control.Reference.Operators where

import Control.Reference.Representation
import Control.Reference.Types
import Control.Reference.Combinators

import Control.Instances.Morph
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List

-- * Getters

-- | Pure getter operator
(^.) :: s -> Getter Identity s a -> a
a ^. l = runIdentity (a ^? l)
infixl 4 ^.

-- | Generic getter operator
(^?) :: Monad m => s -> Getter m s a -> m a
a ^? l = refGet l return a
infixl 4 ^?
  
-- | Gets the context from the referenced element by turning the reference.
review :: Reference MU MU MU Identity s s a a -> a -> s
review r a = a ^. turn r

-- * Setters

-- | Pure setter function
(.=) :: Setter Identity s t a b -> b -> s -> t
l .= v = runIdentity . (l != v)
infixl 4 .=

-- | Monadic setter function
(!=) :: Setter m s t a b -> b -> s -> m t
l != v = refSet l v
infixl 4 !=

-- * Updaters

-- | Monadic updater with a pure result
(.~) :: Setter Identity s t a b -> (a -> Identity b) -> s -> t
l .~ trf = runIdentity . (l !~ trf)
infixl 4 .~

-- | Monadic updater
(!~) :: Setter m s t a b -> (a -> m b) -> s -> m t
l !~ trf = refUpdate l trf
infixl 4 !~

-- * Updaters with pure function inside

-- | Pure updater with pure function
(.-) :: Setter Identity s t a b -> (a -> b) -> s -> t
l .- trf = l .~ return . trf
infixl 4 .-

-- | Monadic update with pure function
(!-) :: Monad m => Setter m s t a b -> (a -> b) -> s -> m t
l !- trf = l !~ return . trf
infixl 4 !-

-- * Updaters with only side-effects

-- | Perform a given action monadically
(!|) :: Monad m => Setter m s s a a -> (a -> m c) -> s -> m s
l !| act = l !~ (\v -> act v >> return v)
infixl 4 !|
