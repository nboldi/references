{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}

-- | Common operators for references. References bind the types of the read and write monads of
-- a reference.
--
-- The naming of the operators follows the given convetions:
--
--  * There are four kinds of operator for every type of reference.
-- The operators are either getters (@^_@), setters (@_=@), monadic updaters (@_~@),
-- pure updaters (@_-@) or action performers (@_|@).
-- The @_@ will be replaced with the signs of the monads accessable.
--
-- * There are pure operators for 'Lens' (@.@), partial operators for 'Partial' lenses (@?@),
-- operators for 'Traversal' (@*@), and operators that work inside 'IO' for 'IOLens' (@!@).
--
-- * Different reference types can be combined, the outermost monad is the first character.
-- Example: Partial IO lens (@?!@). But partial lens and traversal combined is simply a traversal.
--
-- * Generic operators (@#@) do not bind the types of the monads, so they must disambiguated manually.
--

module Control.Reference.Operators where

import Control.Reference.Representation

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
              
-- | Flips a reference to the other direction
turn :: Reference w r w' r' s t a b -> Reference w' r' w r a b s t
turn (Reference refGet refSet refUpdate refGet' refSet' refUpdate')
  = (Reference refGet' refSet' refUpdate' refGet refSet refUpdate)
  
review :: Reference MU MU Identity Identity s t a b -> a -> s
review r a = a ^. turn r

-- * Getters

-- | Gets the referenced data in the monad of the lens.
-- Does not bind the type of the writer monad, so the reference must have its type disambiguated.
(^#) :: RefMonads w r => s -> Reference w r w' r' s t a b -> r a
a ^# l = refGet l return a
infixl 4 ^#

-- | Pure version of '^#'
(^.) :: s -> Lens' s t a b -> a
a ^. l = runIdentity (a ^# l)
infixl 4 ^.

-- | Partial version of '^#'
(^?) :: s -> Partial' s t a b -> Maybe a
a ^? l = a ^# l
infixl 4 ^?

-- | Traversal version of '^#'
(^*) :: s -> Traversal' s t a b -> [a]
a ^* l = a ^# l
infixl 4 ^*

-- | IO version of '^#'
(^!) :: s -> IOLens' s t a b -> IO a
a ^! l = a ^# l
infixl 4 ^!

-- | IO partial version of '^#'
(^?!) :: s -> IOPartial' s t a b -> IO (Maybe a)
a ^?! l = runMaybeT (a ^# l)
infixl 4 ^?!

-- | IO traversal version of '^#'
(^*!) :: s -> IOTraversal' s t a b -> IO [a]
a ^*! l = runListT (a ^# l)
infixl 4 ^*!

-- * Setters

-- | Sets the referenced data to the given pure value in the monad of the reference.
--
-- Does not bind the type of the reader monad, so the reference must have its type disambiguated.
(#=) :: Reference w r w' r' s t a b -> b -> s -> w t
l #= v = refSet l v
infixl 4 #=

-- | Pure version of '#='
(.=) :: Lens' s t a b -> b -> s -> t
l .= v = runIdentity . (l #= v)
infixl 4 .=

-- | Partial version of '#='
(?=) :: Partial' s t a b -> b -> s -> t
l ?= v = runIdentity . (l #= v)
infixl 4 ?=
         
-- | Traversal version of '#='
(*=) :: Traversal' s t a b -> b -> s -> t
l *= v = runIdentity . (l #= v)
infixl 4 *=

-- | IO version of '#='
(!=) :: IOLens' s t a b -> b -> s -> IO t
l != v = l #= v
infixl 4 !=

-- | Partial IO version of '#='
(?!=) :: IOPartial' s t a b -> b -> s -> IO t
l ?!= v = l #= v
infixl 4 ?!=

-- | Traversal IO version of '#='
(*!=) :: IOTraversal' s t a b -> b -> s -> IO t
l *!= v = l #= v
infixl 4 *!=

-- * Updaters

-- | Applies the given monadic function on the referenced data in the monad of the lens.
--
-- Does not bind the type of the reader monad, so the reference must have its type disambiguated.
(#~) :: Reference w r w' r' s t a b -> (a -> w b) -> s -> w t
l #~ trf = refUpdate l trf
infixl 4 #~

-- | Pure version of '#~'
(.~) :: Lens' s t a b -> (a -> Identity b) -> s -> t
l .~ trf = runIdentity . (l #~ trf)
infixl 4 .~

-- | Partial version of '#~'
(?~) :: Partial' s t a b -> (a -> Identity b) -> s -> t
l ?~ trf = runIdentity . (l #~ trf)
infixl 4 ?~

-- | Traversal version of '#~'
(*~) :: Traversal' s t a b -> (a -> Identity b) -> s -> t
l *~ trf = runIdentity . (l #~ trf)
infixl 4 *~

-- | IO version of '#~'
(!~) :: IOLens' s t a b -> (a -> IO b) -> s -> IO t
l !~ trf = l #~ trf
infixl 4 !~

-- | Partial IO version of '#~'
(?!~) :: IOPartial' s t a b -> (a -> IO b) -> s -> IO t
l ?!~ trf = l #~ trf
infixl 4 ?!~

-- | Traversal IO version of '#~'
(*!~) :: IOTraversal' s t a b -> (a -> IO b) -> s -> IO t
l *!~ trf = l #~ trf
infixl 4 *!~

-- * Updaters with pure function inside

-- | Applies the given pure function on the referenced data in the monad of the lens.
--
-- Does not bind the type of the reader monad, so the reference must have its type disambiguated.
(#-) :: Monad w => Reference w r w' r' s t a b -> (a -> b) -> s -> w t
l #- trf = l #~ return . trf
infixl 4 #-

-- | Pure version of '#-'
(.-) :: Lens' s t a b -> (a -> b) -> s -> t
l .- trf = l .~ return . trf
infixl 4 .-

-- | Partial version of '#-'
(?-) :: Partial' s t a b -> (a -> b) -> s -> t
l ?- trf = l ?~ return . trf
infixl 4 ?-

-- | Traversal version of '#-'
(*-) :: Traversal' s t a b -> (a -> b) -> s -> t
l *- trf = l *~ return . trf
infixl 4 *-

-- | IO version of '#-'
(!-) :: IOLens' s t a b -> (a -> b) -> s -> IO t
l !- trf = l !~ return . trf
infixl 4 !-

-- | Partial IO version of '#-'
(?!-) :: IOPartial' s t a b -> (a -> b) -> s -> IO t
l ?!- trf = l ?!~ return . trf
infixl 4 ?!-

-- | Traversal IO version of '#-'
(*!-) :: IOTraversal' s t a b -> (a -> b) -> s -> IO t
l *!- trf = l *!~ return . trf
infixl 4 *!-

-- * Updaters with only side-effects

-- | Performs the given monadic action on referenced data while giving back the original data.
--
-- Does not bind the type of the reader monad, so the reference must have its type disambiguated.
(#|) :: Monad w => Reference w r w' r' s s a a -> (a -> w x) -> s -> w s
l #| act = l #~ (\v -> act v >> return v)
infixl 4 #|

-- | IO version of '#|'
(!|) :: IOLens' s s a a -> (a -> IO c) -> s -> IO s
l !| act = l #| act
infixl 4 !|

-- | Partial IO version of '#|'
(?!|) :: IOPartial' s s a a -> (a -> IO c) -> s -> IO s
l ?!| act = l #| act
infixl 4 ?!|

-- | Traversal IO version of '#|'
(*!|) :: IOTraversal' s s a a -> (a -> IO c) -> s -> IO s
l *!| act = l #| act
infixl 4 *!|

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
-- > a ^* twice == [a,a]
-- > (twice *= x) a == x
-- > (twice *- f) a == f (f a)
--
-- Addition is commutative only if we do not consider the order of the results from a get,
-- or the order in which monadic actions are performed.
--
(&+&) :: (RefMonads w r, RefMonads w' r', MonadPlus r, MonadPlus r', MMorph [] r)
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
