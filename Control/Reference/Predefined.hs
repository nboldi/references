{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase, TupleSections, TypeOperators #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Predefined references for commonly used data structures and reference generators.
--
-- When defining lenses one should use the more general types. For instance 'Lens' instead of the more strict 'Lens''. This way references with different @m1@ and @m2@ monads can be combined if there is a monad @m'@ for @MMorph m1 m'@ and @MMorph m2 m'@.
module Control.Reference.Predefined where

import Control.Reference.Representation
import Control.Reference.Types
import Control.Reference.Generators
import Control.Reference.Combinators
import Control.Reference.Operators

import Control.Instances.Morph
import Control.Monad
import Data.Ratio
import qualified Data.Text as Text
import Data.Complex
import Control.Monad.State
import Control.Concurrent.MVar hiding (modifyMVarMasked_)
import Control.Concurrent.Chan
import Data.IORef
import Data.Either.Combinators
import Data.STRef
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import qualified Control.Exception as Ex

-- * Trivial references

-- | An identical lens. Accesses the context.
--
-- > self & a = a & self = a
self :: Lens a b a b
self = reference return (const . return) id

-- | An empty reference that do not traverse anything
--
-- > emptyRef &+& a = a &+& emptyRef = a
--
-- > a & emptyRef = emptyRef & a = emptyRef
emptyRef :: Simple RefPlus s a
emptyRef = reference (const mzero) (const return) (const return)

-- * References for simple data structures

-- | An indexed lens for accessing points a function
atArg :: Eq a => a -> Simple Lens (a -> b) b
atArg a = lens ($ a) (\b f -> \x -> if a == x then b else f x)

-- | A partial lens to access the value that may not exist
just :: Prism (Maybe a) (Maybe b) a b
just = prism Just Just (maybe (Left Nothing) Right) id

-- | A partial lens to access the right option of an 'Either'
right :: Prism (Either a b) (Either a c) b c
right = prism Right Right (either (Left . Left) Right) rightToMaybe

-- | A partial lens to access the left option of an 'Either'
left :: Prism (Either a c) (Either b c) a b
left = prism Left Left (either Right (Left . Right)) leftToMaybe

-- | Access the value that is in the left or right state of an 'Either'
anyway :: Lens (Either a a) (Either b b) a b
anyway = reference (either return return)
                   (\b -> return . mapBoth (const b) (const b))
                   (\f -> either (f >=> return . Left) (f >=> return . Right))

-- | References both elements of a tuple
both :: Traversal (a,a) (b,b) a b
both = reference (\(x,y)    -> morph [x,y])
                 (\v        -> return . const (v,v))
                 (\f (x,y)  -> (,) <$> f x <*> f y)

-- | References the head of a list
atHead :: Simple Lens [a] (Maybe a)
atHead = lens (\case [] -> Nothing; x:_ -> Just x)
              (\case Nothing -> drop 1;
                     Just v  -> \case []    -> [v]
                                      _:xs  -> v:xs)

-- | References the element at the head of the list
headElem :: Simple Partial [a] a
headElem = atHead & just

-- | References the tail of a list
_tail :: Simple Partial [a] [a]
_tail = simplePartial (\case [] -> Nothing; x:xs -> Just (xs,(x:)))

-- | References a suffix of a list
dropped :: Int -> Simple Partial [a] [a]
dropped 0 = self
dropped i = _tail & dropped (i-1)

-- | Views a list as an optinal pair
view :: Iso [a] [b] (Maybe (a,[a])) (Maybe (b,[b]))
view = iso' to to from from
  where to :: [x] -> Maybe (x,[x])
        to [] = Nothing
        to (x:xs) = Just (x,xs)
        from :: Maybe (x,[x]) -> [x]
        from Nothing = []
        from (Just (x,xs)) = x:xs

-- | An isomorphism between the list and text representation of a string
text :: Simple Iso String Text.Text
text = iso Text.pack Text.unpack

-- | Accesses the reversed version of a list
--
-- > 'turn' reversed == reversed
reversed :: Iso [a] [b] [a] [b]
reversed = iso' reverse reverse reverse reverse

-- | Accesses the numerator of a ratio
_numerator :: Integral a => Simple Lens (Ratio a) a
_numerator = lens numerator (\num' r -> num' % denominator r)

-- | Accesses the denominator of a ratio
_denominator :: Integral a => Simple Lens (Ratio a) a
_denominator = lens denominator (\denom' r -> numerator r % denom')

-- | Accesses the real part of a complex number
_realPart :: RealFloat a => Simple Lens (Complex a) a
_realPart = lens realPart (\real' c -> real' :+ imagPart c)

-- | Accesses the imaginary part of a complex number
_imagPart :: RealFloat a => Simple Lens (Complex a) a
_imagPart = lens imagPart (\imag' c -> realPart c :+ imag')

-- | Accesses the polar representation of a complex number
_polar :: RealFloat a => Simple Lens (Complex a) (a, a)
_polar = iso polar (uncurry mkPolar)

-- * Stateful references

-- | A dummy object to interact with the user through the console.
data Console = Console

-- | Interacts with a line of text on the console. Values set are printed, getting
-- is reading from the console.
consoleLine :: Simple IOLens Console String
consoleLine
  = reference (const (morph getLine))
              (\str -> const (morph (putStrLn str) >> return Console))
              (\f -> const (morph getLine >>= f
                                           >>= morph . putStrLn
                                           >> return Console))

-- | Reference to the contents of the file. Not thread-safe.
--
-- An empty file's content is @Just ""@ while a non-existent file's is @Nothing@
--
-- Creates a temporary file to store the result.
fileContent :: Simple IOLens FilePath (Maybe String)
fileContent
  = reference (\fp -> morph (getFileCont fp))
              (\cont fp -> morph (setFileCont fp cont) >> return fp)
              (\trf fp -> morph (getFile fp) >>= \hcnt -> trf (fmap snd hcnt)
                             >>= morph . updateFileCont fp (fmap fst hcnt) >> return fp)
  where getFileCont :: FilePath -> IO (Maybe String)
        getFileCont fp = (Just <$> readFile fp)
                           `Ex.catch` \e -> if isDoesNotExistError e then return Nothing
                                                                     else Ex.throw e
        getFile :: FilePath -> IO (Maybe (Handle, String))
        getFile fp = do h <- (Just <$> openFile fp ReadMode)
                               `Ex.catch` \e -> if isDoesNotExistError e then return Nothing
                                                                         else Ex.throw e
                        case h of
                          Just handle -> do cont <- hGetContents handle
                                              `Ex.catch` \e -> hClose handle >> Ex.throw (e :: Ex.SomeException)
                                            return $ Just (handle, cont)
                          Nothing -> return Nothing

        setFileCont :: FilePath -> Maybe String -> IO ()
        setFileCont fp Nothing = removeFile fp
        setFileCont fp (Just cont) = writeFile fp cont

        updateFileCont :: FilePath -> Maybe Handle -> Maybe String -> IO ()
        updateFileCont fp h Nothing = (just !| hClose) h >> removeFile fp
        updateFileCont fp h (Just cont)
          = Ex.bracket (openTempFile (takeDirectory fp) (takeFileName fp))
                       (\(tfp,th) -> hClose th >> (just !| hClose) h >> removeFile tfp)
                       (\(_,th) -> hPutStr th cont >> (just !| hClose) h >> hSeek th AbsoluteSeek 0
                                      >> hGetContents th >>= writeFile fp)

-- | Access a value inside an MVar.
-- Setting is not atomic. If there is two supplier that may set the accessed
-- value, one may block and can corrupt the following updates.
--
-- Reads and updates are done in sequence, always using consistent data.
mvar :: Simple IOLens (MVar a) a
mvar = rawReference
         (\f mv -> pullBack $ withMVarMasked mv (sink . f))
         (\newVal mv -> morph $ do empty <- isEmptyMVar mv
                                   if empty then putMVar mv newVal
                                            else swapMVar mv newVal >> return ()
                                   return mv)
         (\trf mv -> modifyMVarMasked_ mv trf >> return mv)
         unusableOp unusableOp unusableOp

-- | Generalized version of 'Control.Concurrent.MVar.modifyMVarMasked_'.
modifyMVarMasked_ :: (Monad m, Morph IO m, MorphControl IO m) => MVar a -> (a -> m a) -> m ()
modifyMVarMasked_ m io =
  mask_ $ do
    a  <- morph (takeMVar m)
    a' <- io a `onException` morph (putMVar m a)
    morph (putMVar m a')

-- | Generalized version of 'Control.Exception.mask_'.
mask_ :: (MorphControl IO m) => m a -> m a
mask_ = pullBack . Ex.mask_ . sink

-- | Generalized version of 'Control.Exception.onException'.
onException :: (MorphControl IO m) => m a -> m b -> m a
onException a b = pullBack $ Ex.onException (sink a) (sink b)

chan :: Simple IOLens (Chan a) a
chan = reference (morph . readChan)
                 (\a ch -> morph (writeChan ch a) >> return ch)
                 (\trf ch -> morph (readChan ch) >>= trf
                               >>= morph . writeChan ch >> return ch)

-- | Access the value of an IORef.
ioref :: Simple IOLens (IORef a) a
ioref = reference (morph . readIORef)
                  (\v ior -> morph (atomicWriteIORef ior v) >> return ior)
                  (\trf ior -> morph (readIORef ior)
                                 >>= trf >>= morph . writeIORef ior >> return ior)

-- | Access the state inside a state monad (from any context).
state :: forall s m a . Monad m => Simple (StateLens s m) a s
state = reference (morph . const get') (\a s -> morph (put' a) >> return s)
                  (\trf s -> (morph get' >>= trf >> return s))
  where put' = put :: s -> StateT s m ()
        get' = get :: StateT s m s

-- | Access the value inside an 'STRef'
stRef :: Simple (STLens s) (STRef s a) a
stRef = reference (morph . readSTRef)
                  (\newVal ref -> morph $ writeSTRef ref newVal >> return ref)
                  (\trf ref -> morph (readSTRef ref) >>= trf
                                 >>= morph . writeSTRef ref >> return ref)

-- | Filters an indexed reference based on the index
whereOf :: (RefMonads w r, MonadPlus r)
        => (i -> Bool) -> (IndexedReference i w r MU MU s s a a) -> (IndexedReference i w r MU MU s s a a)
whereOf p iref i | p i       = iref i
                 | otherwise = emptyRef
