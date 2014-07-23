{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

-- | Common operators for references
module Control.Reference.Operators where

import Control.Reference.Representation

import Control.Monad.Identity
        
infixl 4 .~
infixl 4 .=
infixl 4 %~
infixl 4 %~=
infixl 4 %=
infixl 4 ^.
infixl 4 ^?
        
-- | Gets the referenced data
(^.) :: s -> Reference wm Identity s t a b -> a
a ^. l = runIdentity (a ^? l)

-- | Gets the referenced data in the reader monad of the lens
(^?) :: s -> Reference wm rm s t a b -> rm a
a ^? l = lensGet l a
        
-- | Sets the referenced data (for lenses with identity writer)
(.~) :: Reference Identity rm s t a b -> b -> (s -> t)
l .~ v = runIdentity . (l .= v)

-- | Sets the referenced data in the writer monad of the lens
(.=) :: Monad rw => Reference rw rm s t a b -> b -> (s -> rw t)
l .= v = lensSet l v 

-- | Applies the given function on the referenced data (for lenses with identity writer)
(%~) :: Reference Identity rm s t a b -> (a -> b) -> (s -> t)
l %~ trf = runIdentity . lensUpdate l (return . trf)

-- | Applies the given monadic function on the referenced data in the monad of the lens
(%~=) :: Monad rw => Reference rw rm s t a b -> (a -> b) -> (s -> rw t)
l %~= trf = lensUpdate l (return . trf)

-- | Applies the given monadic function on the referenced data in the monad of the lens
(%=) :: Reference rw rm s t a b -> (a -> rw b) -> (s -> rw t)
l %= trf = lensUpdate l trf

-- | Performs the given monadic action on referenced data
(%!) :: Monad rw => Reference rw rm s s a a -> (a -> rw c) -> (s -> rw s)
l %! act = lensUpdate l (\v -> act v >> return v)
            
-- | Composes two references. The two references should have the same writer semantics 
-- and their reader semantics must be composable with 'MonadCompose'.
(&) :: forall w r1 r2 s t c d a b . ( MonadCompose r1 r2 ) 
    => Reference w r1 s t c d -> Reference w r2 c d a b
    -> Reference w (ResultMonad r1 r2) s t a b
(&) l1 l2 = Reference (\s -> (liftMC1 phr (lensGet l1 s)) >>= (liftMC2 phr . lensGet l2)) 
                      (lensUpdate l1 . lensSet l2) 
                      (lensUpdate l1 . lensUpdate l2)
  where phr = newComposePhantom
  
infixl 6 &

-- | Adds two references. 
-- The references must be monomorphic, because setter needs
-- to change the object twice.
(&+&) :: forall w r1 r2 r12 r3 a s
       . (Monad w, MonadPlus r3, MonadCompose r1 r2, r12 ~ ResultMonad r1 r2
                               , MonadCompose r12 [], r3 ~ (ResultMonad r12 []))
      => Reference w r1 s s a a -> Reference w r2 s s a a
      -> Reference w r3 s s a a
l1 &+& l2 = Reference (\a -> liftMC1 cf2 (liftMC1 cf1 (a ^? l1))
                                `mplus` liftMC1 cf2 (liftMC2 cf1 (a ^? l2))) 
                      (\v a -> (l1 .= v) a >>= l2 .= v )
                      (\trf a -> (l1 %= trf) a >>= (l2 %= trf) )
    where cf1 = newComposePhantom
          cf2 = newComposePhantom :: ComposePhantom r12 []
          
infixl 5 &+&
