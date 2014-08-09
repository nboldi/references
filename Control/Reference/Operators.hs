{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}

-- | Common operators for references
module Control.Reference.Operators where
import Control.Reference.Representation
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Applicative

infixl 4 #=
infixl 4 .=
infixl 4 ?=
infixl 4 *=
infixl 4 !=
infixl 4 ?!=
infixl 4 *!=

infixl 4 #~
infixl 4 .~
infixl 4 ?~
infixl 4 *~
infixl 4 !~
infixl 4 ?!~
infixl 4 *!~

infixl 4 #-
infixl 4 .-
infixl 4 ?-
infixl 4 *-
infixl 4 !-
infixl 4 ?!-
infixl 4 *!-

infixl 4 #|
infixl 4 !|
infixl 4 ?!|
infixl 4 *!|

infixl 4 ^#
infixl 4 ^.
infixl 4 ^?
infixl 4 ^*
infixl 4 ^!
infixl 4 ^?!
infixl 4 ^*!

-- * Getters

-- | Gets the referenced data in the monad of the lens
(^#) :: (Monad m, Applicative m, Functor m)
        => s -> Reference m s t a b -> m a
a ^# l = refGet l a <* getClose l a

-- | Pure version of '^#'
(^.) :: s -> Lens' s t a b -> a
a ^. l = runIdentity (a ^# l)

-- | Partial version of '^#'
(^?) :: s -> LensPart' s t a b -> Maybe a
a ^? l = a ^# l

-- | Traversal version of '^#'
(^*) :: s -> Traversal' s t a b -> [a]
a ^* l = a ^# l

-- | IO version of '^#'
(^!) :: s -> RefIO' s t a b -> IO a
a ^! l = a ^# l

-- | Partial IO version of '^#'
(^?!) :: s -> PartIO' s t a b -> MaybeT IO a
a ^?! l = a ^# l

-- | Traversal IO version of '^#'
(^*!) :: s -> TravIO' s t a b -> ListT IO a
a ^*! l = a ^# l

-- * Setters
          
-- | Sets the referenced data in the monad of the reference
(#=) :: (Monad m, Applicative m, Functor m)
        => Reference m s t a b -> b -> s -> m t
l #= v = \s -> refSet l v s <* setClose l s

-- | Setter for lenses
(.=) :: Lens' s t a b -> b -> s -> t
l .= v = runIdentity . (l #= v)

-- | Setter for partial lenses
(?=) :: LensPart' s s a a -> a -> s -> s
l ?= v = summarize (l #= v)
                
-- | Setter for traversals
(*=) :: Traversal' s s a a -> a -> s -> s
l *= v = summarize (l #= v)

-- | Setter for IO. Cannot be summarized.
(!=) :: RefIO' s t a b -> b -> s -> IO t
l != v = l #= v

-- | Setter for Partial IO.
(?!=) :: PartIO' s s a a -> a -> s -> IO s
l ?!= v = summarizeInto (l #= v)

-- | Setter for Traversal IO.
(*!=) :: TravIO' s s a a -> a -> s -> IO s
l *!= v = summarizeInto (l #= v)

-- * Updaters

-- | Applies the given monadic function on the referenced data in the monad of the lens
(#~) :: (Monad m, Applicative m, Functor m)
        => Reference m s t a b -> (a -> m b) -> s -> m t
l #~ trf = \s -> refUpdate l trf s <* updateClose l s

(.~) :: Lens' s t a b -> (a -> Identity b) -> s -> t
l .~ trf = runIdentity . (l #~ trf)

(?~) :: LensPart' s s a a -> (a -> Maybe a) -> s -> s
l ?~ trf = summarize (l #~ trf)

(*~) :: Traversal' s s a a -> (a -> [a]) -> s -> s
l *~ trf = summarize (l #~ trf)

(!~) :: RefIO' s t a b -> (a -> IO b) -> s -> IO t
l !~ trf = l #~ trf

(?!~) :: PartIO' s s a a -> (a -> MaybeT IO a) -> s -> IO s
l ?!~ trf = summarizeInto (l #~ trf)

(*!~) :: TravIO' s s a a -> (a -> ListT IO a) -> s -> IO s
l *!~ trf = summarizeInto (l #~ trf)

-- * Updaters with pure function inside

-- | Applies the given monadic function on the referenced data in the monad of the lens
(#-) :: (Monad m, Applicative m, Functor m)
        => Reference m s t a b -> (a -> b) -> s -> m t
l #- trf = l #~ return . trf

(.-) :: Lens' s t a b -> (a -> b) -> s -> t
l .- trf = l .~ return . trf

(?-) :: LensPart' s s a a -> (a -> a) -> s -> s
l ?- trf = l ?~ return . trf

(*-) :: Traversal' s s a a -> (a -> a) -> s -> s
l *- trf = l *~ return . trf

(!-) :: RefIO' s t a b -> (a -> b) -> s -> IO t
l !- trf = l !~ return . trf

(?!-) :: PartIO' s s a a -> (a -> a) -> s -> IO s
l ?!- trf = l ?!~ return . trf

(*!-) :: TravIO' s s a a -> (a -> a) -> s -> IO s
l *!- trf = l *!~ return . trf

-- * Updaters with only side-effects

-- | Performs the given monadic action on referenced data and gives the original data back
(#|) :: (Monad m, Applicative m, Functor m)
        => Reference m s s a a -> (a -> m x) -> s -> m s
l #| act = l #~ (\v -> act v >> return v)

(!|) :: RefIO' s s a a -> (a -> IO c) -> s -> IO s
l !| act = l #| act

(?!|) :: PartIO' s s a a -> (a -> MaybeT IO c) -> s -> IO s
l ?!| act = summarizeInto (l #| act)

(*!|) :: TravIO' s s a a -> (a -> ListT IO c) -> s -> IO s
l *!| act = summarizeInto (l #| act)


-- * Binary operators on references

-- | Composes two references.
(&) :: (CloseMonad m) => Reference m s t c d -> Reference m c d a b
    -> Reference m s t a b
(&) l1 l2 = Reference (refGet l1 >=> refGet l2) 
                      (refUpdate l1 . refSet l2) 
                      (refUpdate l1 . refUpdate l2)
                      (\s -> normalizeClose $ getClose l1 s >> refGet l1 s >>= getClose l2)
                      (\s -> normalizeClose $ setClose l1 s >> refGet l1 s >>= setClose l2)
                      (\s -> normalizeClose $ updateClose l1 s >> refGet l1 s >>= updateClose l2)
  
infixl 6 &

-- | Adds two references.
(&+&) :: (CloseMonad m, MonadPlus m, [] !<! m)
         => Reference m s s a a -> Reference m s s a a
         -> Reference m s s a a
l1 &+& l2 = Reference (\a -> refGet l1 a `mplus` refGet l2 a) 
                      (\v -> refSet l1 v >=> refSet l2 v )
                      (\trf -> refUpdate l1 trf
                                 >=> refUpdate l2 trf )
                      (\s -> normalizeClose $ getClose l1 s >> getClose l2 s)
                      (\s -> normalizeClose $ setClose l1 s >> setClose l2 s)
                      (\s -> normalizeClose $ updateClose l1 s >> updateClose l2 s)
          
infixl 5 &+&

