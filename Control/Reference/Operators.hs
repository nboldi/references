{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

-- | Common operators for references
module Control.Reference.Operators where
import Control.Reference.Representation
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List

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
infixl 4 .|
infixl 4 ?|
infixl 4 *|
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
(^#) :: s -> Reference m s t a b -> m a
a ^# l = lensGet l a

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
(#=) :: Monad m => Reference m s t a b -> b -> s -> m t
l #= v = lensSet l v 

-- | Setter for lenses
(.=) :: Lens' s s a a -> a -> s -> s
l .= v = summarize (l #= v)

-- | Setter for partial lenses
(?=) :: LensPart' s s a a -> a -> s -> s
l ?= v = summarize (l #= v)

-- | Setter for traversals
(*=) :: Traversal' s s a a -> a -> s -> s
l *= v = summarize (l #= v)

-- | Setter for IO. Cannot be summarized.
(!=) :: RefIO' s s a a -> a -> s -> IO s
l != v = l #= v

-- | Setter for Partial IO.
(?!=) :: PartIO' s s a a -> a -> s -> IO s
l ?!= v = summarizeM (runMaybeT . (l #= v))

-- | Setter for Traversal IO.
(*!=) :: TravIO' s s a a -> a -> s -> IO s
l *!= v = summarizeM (runListT . (l #= v))

-- * Updaters

-- | Applies the given monadic function on the referenced data in the monad of the lens
(#~) :: Reference m s t a b -> (a -> m b) -> s -> m t
l #~ trf = lensUpdate l trf

(.~) :: Lens' s s a a -> (a -> Identity a) -> s -> s
l .~ trf = summarize (l #~ trf)

(?~) :: LensPart' s s a a -> (a -> Maybe a) -> s -> s
l ?~ trf = summarize (l #~ trf)

(*~) :: Traversal' s s a a -> (a -> [a]) -> s -> s
l *~ trf = summarize (l #~ trf)

(!~) :: RefIO' s s a a -> (a -> IO a) -> s -> IO s
l !~ trf = l #~ trf

(?!~) :: PartIO' s s a a -> (a -> MaybeT IO a) -> s -> IO s
l ?!~ trf = summarizeM (runMaybeT . (l #~ trf))

(*!~) :: TravIO' s s a a -> (a -> ListT IO a) -> s -> IO s
l *!~ trf = summarizeM (runListT . (l #~ trf))

-- * Updaters with pure function inside

-- | Applies the given monadic function on the referenced data in the monad of the lens
(#-) :: Monad m => Reference m s t a b -> (a -> b) -> s -> m t
l #- trf = l #~ return . trf

(.-) :: Lens' s s a a -> (a -> a) -> s -> s
l .- trf = l .~ return . trf

(?-) :: LensPart' s s a a -> (a -> a) -> s -> s
l ?- trf = l ?~ return . trf

(*-) :: Traversal' s s a a -> (a -> a) -> s -> s
l *- trf = l *~ return . trf

(!-) :: RefIO' s s a a -> (a -> a) -> s -> IO s
l !- trf = l !~ return . trf

(?!-) :: PartIO' s s a a -> (a -> a) -> s -> IO s
l ?!- trf = l ?!~ return . trf

(*!-) :: TravIO' s s a a -> (a -> a) -> s -> IO s
l *!- trf = l *!~ return . trf

-- * Updaters with only side-effects

-- | Performs the given monadic action on referenced data and gives it back
(#|) :: Monad m => Reference m s s a a -> (a -> m x) -> s -> m s
l #| act = l #~ (\v -> act v >> return v)

(.|) :: Lens' s s a a -> (a -> Identity c) -> s -> Identity s
l .| act = l #| act

(?|) :: LensPart' s s a a -> (a -> Maybe c) -> s -> Maybe s
l ?| act = l #| act

(*|) :: Traversal' s s a a -> (a -> [a]) -> s -> [s]
l *| act = l #| act

(!|) :: RefIO' s s a a -> (a -> IO a) -> s -> IO s
l !| act = l #| act

(?!|) :: PartIO' s s a a -> (a -> MaybeT IO a) -> s -> MaybeT IO s
l ?!| act = l #| act

(*!|) :: TravIO' s s a a -> (a -> ListT IO a) -> s -> ListT IO s
l *!| act = l #| act


-- * Binary operators on references

-- | Composes two references.
(&) :: (Monad m) => Reference m s t c d -> Reference m c d a b
    -> Reference m s t a b
(&) l1 l2 = Reference (lensGet l1 >=> lensGet l2) 
                      (lensUpdate l1 . lensSet l2) 
                      (\trf s -> lensUpdate l1 (lensUpdate l2 trf) s)
  
infixl 6 &

-- | Adds two references.
(&+&) :: (MonadPlus m) => Reference m s s a a -> Reference m s s a a
      -> Reference m s s a a
l1 &+& l2 = Reference (\a -> lensGet l1 a `mplus` lensGet l2 a) 
                      (\v a -> lensSet l1 v a `mplus` lensSet l2 v a )
                      (\trf a -> lensUpdate l1 trf a
                                   `mplus` lensUpdate l2 trf a )
          
infixl 5 &+&


-- instance MonadSubsume Maybe Maybe where
--   liftMS = id

-- instance MonadSubsume [] [] where
--   liftMS = id
