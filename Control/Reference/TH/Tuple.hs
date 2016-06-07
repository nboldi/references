{-# LANGUAGE TemplateHaskell #-}
-- | A module for creating lenses to fields of simple, tuple data structures 
-- like pairs, triplets, and so on.
module Control.Reference.TH.Tuple (TupleConf(..), hsTupConf, makeTupleRefs) where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative
import Data.Maybe

import Control.Reference.InternalInterface

-- | Creates @Lens_1@ ... @Lens_n@ classes, and instances for tuples up to 'm'.
-- 
-- Classes and instances look like the following:
-- 
-- @
-- class Lens_1 s t a b | s -> a, t -> b
--                      , a t -> s, b s -> t where 
--   _1 :: Lens s t a b
--
-- instance Lens_1 (a,b) (a',b) a a' where 
--   _1 = lens (\(a,b) -> a) (\a' (a,b) -> (a',b))
-- @
--
makeTupleRefs :: TupleConf -> Int -> Int -> Q [Dec]
makeTupleRefs conf n m 
  = (++) <$> (catMaybes <$> genClass `mapM` [0..(n-1)]) 
         <*> (genInstance conf 
                  `mapM` [ (x, y) | x <- [0..(n-1)]
                                  , y <- [(max 2 (x+1))..m] ])             

genClass :: Int -> Q (Maybe Dec)
genClass i 
  = do declared <- classDeclared i
       if declared then return Nothing
                   else Just <$> genClass' i
  where genClass' i = 
          do s <- newName "s"
             t <- newName "t"
             a <- newName "a"
             b <- newName "b1"
             let tvars = map PlainTV [s,t,a,b]
             return $ ClassD [] (lensClass i) tvars
                             [ FunDep [s] [a], FunDep [t] [b]
                             , FunDep [a,t] [s], FunDep [b,s] [t]] 
                             [ SigD (lensFun i) 
                                    (foldl AppT (ConT ''Lens) (map VarT [s,t,a,b]))         
                             ]    

lensClass i = mkName ("Lens_" ++ show (i+1))
lensFun i = mkName ("_" ++ show (i+1))
  
classDeclared :: Int -> Q Bool 
classDeclared i = isJust <$> lookupTypeName (nameBase $ lensClass i)

genInstance :: TupleConf -> (Int,Int) -> Q Dec
genInstance (TupleConf typGen patGen expGen) (n,m)
  = do names <- replicateM m (newName "a")
       name <- newName "b2"
       genBody <- generateBody
       return $ InstanceD Nothing [] 
                          (ConT (lensClass n) 
                             `AppT` typGen names
                             `AppT` typGen (replace n name names)
                             `AppT` VarT (names !! n)
                             `AppT` VarT name
                          ) 
                          [ ValD (VarP (lensFun n) ) 
                                 (NormalB genBody) [] ]

  where generateBody :: Q Exp
        generateBody
          = do names <- replicateM m (newName "a")
               name <- newName "b3"
               return $ VarE 'lens 
                          `AppE` LamE [patGen names] 
                                      (VarE (names !! n))
                          `AppE` LamE [VarP name, patGen names] 
                                      (expGen (replace n name names))

-- | A tuple configuration is a scheme for tuple-like data structures.
data TupleConf = TupleConf { tupleType      :: [Name] -> Type
                           , tuplePattern   :: [Name] -> Pat
                           , tupleExpr      :: [Name] -> Exp
                           }
        
-- | Generates the normal haskell tuples (@(a,b), (a,b,c), (a,b,c,d)@)        
hsTupConf 
  = TupleConf (\names -> foldl AppT (TupleT (length names)) . map VarT $ names) 
              (TupP . map VarP) 
              (TupE . map VarE)
                  
-- | Utility function to replace the N'th element of a list                         
replace :: Int -> a -> [a] -> [a]
replace i e ls 
  = let (before,after) = splitAt i ls 
     in case after of [] -> error $ "replace : Index " ++ show i ++ " is not found." 
                      _:rest -> before ++ e : rest
 
