{-# LANGUAGE FlexibleContexts #-}

-- | Operators to combine and transform references.
module Control.Reference.Combinators where

import Control.Reference.Representation
import Control.Instances.Morph
import Control.Monad
import Control.Monad.Identity
import Control.Applicative

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

              
-- | Flips a reference to the other direction.
-- The monads of the references can change when a reference is turned.
turn :: Reference w r w' r' s t a b -> Reference w' r' w r a b s t
turn (Reference refGet refSet refUpdate refGet' refSet' refUpdate')
  = (Reference refGet' refSet' refUpdate' refGet refSet refUpdate)
