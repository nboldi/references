{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase, TupleSections, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, TypeFamilies, FunctionalDependencies, LiberalTypeSynonyms #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif


-- | Predefined references for commonly used data structures and reference generators.
--
-- When defining lenses one should use the more general types. For instance 'Lens' instead of the more strict 'Lens''. This way references with different @m1@ and @m2@ monads can be combined if there is a monad @m'@ for @MMorph m1 m'@ and @MMorph m2 m'@.
module Control.Reference.Predefined where

import Control.Reference.Representation
import Control.Reference.Operators

import Control.Applicative
import Control.Monad
import qualified Data.Traversable as Trav
import Data.Ratio
import qualified Data.Text as Text
import Data.Complex
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Concurrent.MVar.Lifted
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


-- * Reference generators

-- | Generates a traversal for any 'Trav.Traversable' 'Functor'
traverse :: (Trav.Traversable t) => Traversal (t a) (t b) a b
traverse = reference (morph . execWriter . Trav.mapM (tell . (:[])))
                     (Trav.mapM . const . return) 
                     Trav.mapM
             
-- | Generate a lens from a pair of inverse functions
iso :: (a -> b) -> (b -> a) -> Simple Iso a b
iso f g = bireference (return . f) (\b _ -> return . g $ b) (\trf a -> trf (f a) >>= return . g  ) 
                      (return . g) (\a _ -> return . f $ a) (\trf b -> trf (g b) >>= return . f  ) 
             
iso' :: (a -> b) -> (a' -> b') -> (b -> a) -> (b' -> a') -> Iso a a' b b'
iso' f f' g g' 
  = bireference (return . f) (\b _ -> return . g' $ b) (\trf a -> trf (f a) >>= return . g'  ) 
                (return . g) (\a _ -> return . f' $ a) (\trf b -> trf (g b) >>= return . f'  ) 

-- | Generates a lens from a getter and a setter
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens get set = reference (return . get) 
                         (\b -> return . set b ) 
                         (\f a -> f (get a) >>= \b -> return $ set b a)

-- | Creates a polymorphic partial lense
--
-- @Either t a@ is used instead of @Maybe a@ to permit the types of 's' and 't' to differ.
partial :: (s -> Either t (a, b -> t)) -> Partial s t a b
partial access 
  = reference 
      (either (const $ morph Nothing) (return . fst) . access)
      (\b -> return . either id (($b) . snd) . access)
      (\f -> either return (\(a,set) -> f a >>= return . set) . access)
         
-- | Creates a polymorphic partial lens that can be turned to give a total lens
prism :: (a -> s) -> (b -> t) -> (s -> Either t a) -> (t -> Maybe b) -> Prism s t a b
prism back back' access access'
  = bireference (either (const $ morph Nothing) return . access)
                (\b -> return . either id (const $ (back' b)) . access)
                (\f -> either return (f >=> return . back') . access)
                (return . back)
                (\t _ -> morph $ access' t)
                (\f a -> f (back a) >>= morph . access')
                
-- | Creates a monomorphic partial lens that can be turned to give a total lens
simplePrism :: (a -> s) -> (s -> Maybe a) -> Prism s s a a
simplePrism back access = prism back back (\s -> maybe (Left s) Right (access s)) access
                
-- | Creates a simple partial lens
simplePartial :: (s -> Maybe (a, a -> s)) -> Partial s s a a
simplePartial access 
  = partial (\s -> maybe (Left s) Right (access s))

                                                     
-- | Clones a lens from "Control.Lens"
fromLens :: (forall f . Functor f => (a -> f b) -> s -> f t) -> Lens s t a b
fromLens l = reference (\s -> return (getConst $ l Const s))
                       (\b -> return . (runIdentity . l (\_ -> Identity b)))
                       l
                 
-- | Clones a traversal from "Control.Lens"
fromTraversal :: (forall f . Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
fromTraversal l = reference (morph . execWriter . l (\a -> tell [a] >> return undefined))
                            (\b -> return . (runIdentity . l (\_ -> Identity b)))
                            l

-- | Filters the traversed elements with a given predicate. 
-- Has specific versions for traversals and partial lenses.
filtered :: (a -> Bool) -> Simple RefPlus a a
filtered p = reference (\s -> if p s then return s else mzero)
                       (\a s -> if p s then return a else return s)
                       (\f s -> if p s then f s else return s)

-- * References for simple data structures

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
        updateFileCont fp h Nothing = (just ?!| hClose) h >> removeFile fp 
        updateFileCont fp h (Just cont) 
          = Ex.bracket (openTempFile (takeDirectory fp) (takeFileName fp))
                       (\(tfp,th) -> hClose th >> (just ?!| hClose) h >> removeFile tfp)
                       (\(_,th) -> hPutStr th cont >> (just ?!| hClose) h >> hSeek th AbsoluteSeek 0 
                                      >> hGetContents th >>= writeFile fp)
                                    
               
-- | Access a value inside an MVar.
-- Setting is not atomic. If there is two supplier that may set the accessed
-- value, one may block and can corrupt the following updates.
--
-- Reads and updates are done in sequence, always using consistent data.
mvar :: Simple IOLens (MVar a) a
mvar = rawReference 
         (flip withMVarMasked)
         (\newVal mv -> do empty <- isEmptyMVar mv
                           if empty then putMVar mv newVal
                                    else swapMVar mv newVal >> return ()
                           return mv)
         (\trf mv -> modifyMVarMasked_ mv trf >> return mv)     
         (\_ _ -> MU) (\_ _ -> MU) (\_ _ -> MU)


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
 
