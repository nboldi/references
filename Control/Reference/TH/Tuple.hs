{-# LANGUAGE TemplateHaskell #-}
-- | A module for making connections between different monads.
module Control.Reference.TH.Tuple (makeTupleRefs) where

import Language.Haskell.TH
import Control.Monad

import Control.Reference.Representation

-- | Creates @_1@ ... @_n@ classes, and instances for tuples up to m
makeTupleRefs :: Int -> Int -> Q [Dec]
makeTupleRefs n m 
  = liftM2 (++) (genClass `mapM` [0..(n-1)]) 
                (genInstance `mapM` [ (x, y) | x <- [0..(n-1)], y <- [(max 2 (x+1))..m] ])
      -- >>= runIO . putStrLn . pprint >> return []
             
     

genClass :: Int -> Q Dec
genClass i 
  = do s <- newName "s"
       t <- newName "t"
       a <- newName "a"
       b <- newName "b1"
       let tvars = map PlainTV [s,t,a,b]
       return $ ClassD [] (mkName ("Lens_" ++ show (i+1))) tvars
                       [ FunDep [s] [a], FunDep [t] [b]
                       , FunDep [a,t] [s], FunDep [b,s] [t]] 
                       [ SigD normalLens 
                              ( ForallT [] [] (foldl AppT (ConT ''Lens) (map VarT [s,t,a,b])) )               
                       ]    
  where normalLens = mkName ("_" ++ show (i+1))
        

genInstance :: (Int,Int) -> Q Dec
genInstance (n,m)
  = do names <- replicateM m (newName "a")
       name <- newName "b2"
       genBody <- generateBody
       return $ InstanceD [] (ConT (mkName ("Lens_" ++ show (n+1))) 
                                `AppT` foldl AppT (TupleT m) (map VarT names)
                                `AppT` foldl AppT (TupleT m) (map VarT (replace n name names))
                                `AppT` VarT (names !! n)
                                `AppT` VarT name
                             ) 
                             [ ValD (VarP (mkName ("_" ++ show (n+1)))) 
                                    (NormalB genBody) [] ]

  where generateBody :: Q Exp
        generateBody 
          = do names <- replicateM m (newName "a")
               name <- newName "b3"
               trf <- newName "trf"
               return $ ConE 'Reference 
                          `AppE` LamE [TupP (map VarP names)] 
                                      (VarE 'return `AppE` VarE (names !! n))
                          `AppE` LamE [VarP name, TupP (map VarP names)] 
                                      (VarE 'return `AppE` TupE (map VarE (replace n name names)))
                          `AppE` LamE [VarP trf, TupP (map VarP names)] 
                                      (VarE 'liftM 
                                        `AppE` LamE [VarP name] (TupE (map VarE (replace n name names))) 
                                        `AppE` (VarE trf `AppE` VarE (names !! n)))
                                    
replace :: Int -> a -> [a] -> [a]
replace i e ls 
  = let (before,after) = splitAt i ls 
     in case after of [] -> error $ "replace : Index " ++ show i ++ " is not found." 
                      _:rest -> before ++ e : rest
 
