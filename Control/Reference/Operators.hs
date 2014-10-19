{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}

--
-- | Common operators for using, transforming and combining.
--
-- There are four kinds of operator for every type of reference.
-- The operators are either getters ('^.' and '^?'), setters ('.=' and '!='), 
-- monadic updaters ('.~' and '!~'), pure updaters ('.-' and '!-') or action performers (@!|@).
--
-- The former operators (with the dot) are pure operators, the later are monadic operators. For example, @(1,2) ^. _1@ results in a pure numeric value, while @Right 4 ^? right@ produces @Just 4@ (or a higher level value representing that).
--

module Control.Reference.Operators where

import Control.Reference.Representation

import Control.Instances.Morph
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
              
-- | Flips a reference to the other direction.
-- The monads of the references can change when a reference is turned.
turn :: Reference w r w' r' s t a b -> Reference w' r' w r a b s t
turn (Reference refGet refSet refUpdate refGet' refSet' refUpdate')
  = (Reference refGet' refSet' refUpdate' refGet refSet refUpdate)
  
-- | Gets the context from the referenced element by turning the reference.
review :: Reference MU MU MU Identity s s a a -> a -> s
review r a = a ^. turn r

-- * Getters

-- | Pure getter operator
(^.) :: s -> Getter Identity s a -> a
a ^. l = runIdentity (a ^? l)
infixl 4 ^.

-- | Generic getter operator
(^?) :: Monad m => s -> Getter m s a -> m a
a ^? l = refGet l return a
infixl 4 ^?

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

-- * Binary operators on references

-- | Composes two references. They must be of the same kind.
--
-- If reference @r@ accesses @b@ inside the context @a@, and reference @p@ accesses @c@ inside the context @b@,
-- than the reference @r&p@ will access @c@ inside @a@.
--
-- Composition is associative: @ (r&p)&q = r&(p&q) @
(&) :: (Monad w, Monad r) => Reference w r w' r' s t c d -> Reference w r w' r' c d a b
    -> Reference w r w' r' s t a b
(&) l1 l2 = Reference (refGet l1 . refGet l2) 
                      (refUpdate l1 . refSet l2) 
                      (refUpdate l1 . refUpdate l2)
                      (refGet' l2 . refGet' l1)
                      (refUpdate' l2 . refSet' l1) 
                      (refUpdate' l2 . refUpdate' l1)
infixl 6 &

-- | Adds two references.
--
-- Using this operator may result in accessing the same parts of data multiple times.
-- For example @ twice = self &+& self @ is a reference that accesses itself twice:
--
-- > a ^? twice == [a,a]
-- > (twice *= x) a == x
-- > (twice .- f) a == f (f a)
--
-- Addition is commutative only if we do not consider the order of the results from a get,
-- or the order in which monadic actions are performed.
--
(&+&) :: (RefMonads w r, RefMonads w' r', MonadPlus r, MonadPlus r', Morph [] r)
         => Reference w r w' r' s s a a -> Reference w r w' r' s s a a
         -> Reference w r w' r' s s a a
l1 &+& l2 = Reference (\f a -> refGet l1 f a `mplus` refGet l2 f a) 
                      (\v -> refSet l1 v >=> refSet l2 v )
                      (\trf -> refUpdate l1 trf
                                 >=> refUpdate l2 trf )
                      (\f a -> refGet' l1 f a `mplus` refGet' l2 f a) 
                      (\v -> refSet' l1 v >=> refSet' l2 v )
                      (\trf -> refUpdate' l1 trf
                                 >=> refUpdate' l2 trf )
infixl 5 &+&

-- | Pack two references in parallel.
(&|&) :: (RefMonads m m') 
      => Reference m m m' m' s t a b -> Reference m m m' m' s' t' a' b' 
           -> Reference m m m' m' (s, s') (t, t') (a, a') (b, b')
r1 &|& r2 = Reference (\f (s1,s2) -> ((,) <$> refGet r1 return s1 <*> refGet r2 return s2) >>= f) 
                      (\(b1,b2) (s1,s2) -> (,) <$> refSet r1 b1 s1 <*> refSet r2 b2 s2) 
                      (\f (s1,s2) -> do a1 <- refGet r1 return s1
                                        a2 <- refGet r2 return s2
                                        t1 <- refUpdate r1 (liftM fst . flip (curry f) a2) s1
                                        t2 <- refUpdate r2 (liftM snd . curry f a1) s2
                                        return (t1, t2) ) 
                      (\f (s1,s2) -> ((,) <$> refGet' r1 return s1 <*> refGet' r2 return s2) >>= f)
                      (\(b1,b2) (s1,s2) -> (,) <$> refSet' r1 b1 s1 <*> refSet' r2 b2 s2)
                      (\f (s1,s2) -> do a1 <- refGet' r1 return s1
                                        a2 <- refGet' r2 return s2
                                        t1 <- refUpdate' r1 (liftM fst . flip (curry f) a2) s1
                                        t2 <- refUpdate' r2 (liftM snd . curry f a1) s2
                                        return (t1, t2) )

infixl 5 &|&
