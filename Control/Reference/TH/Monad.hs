{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, DoAndIfThenElse #-}

-- | A module for making connections between different monads.
module Control.Reference.TH.Monad (makeMonadRepr) where

import Control.Reference.Representation
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.TH

class ToQType t where
  toQType :: t -> Q Type
  
instance ToQType Type where 
  toQType = return    
instance ToQType (Q Type) where 
  toQType = id  
instance ToQType Name where 
  toQType = return . ConT  
  
class ToQExp t where
  toQExp :: t -> Q Exp
  
instance ToQExp (Q Exp) where 
  toQExp = id  
instance ToQExp Name where 
  toQExp = return . VarE

type IGState m a = StateT InstanceGenState m a
  
data InstanceGenState = IGS { subsumeInsts :: [(Type, Type)]
                            , composeInsts :: [(Type, Type)]            
                            } deriving Show
   
-- | Creates 'MonadSubsume' and 'MonadCompose' instances that can be inferred from a single subsume 
-- connection and all instances declared so far.
makeMonadRepr :: (ToQType t1, ToQType t2, ToQExp e) 
              => t1 -> t2 -> e -> Q [Dec]
makeMonadRepr m1' m2' e'
  = do t1 <- toQType m1'; t2 <- toQType m2'; e <- toQExp e' 
       ClassI _ subsumeInstances <- reify ''MonadSubsume
       let subsumes = map (\(InstanceD _ (AppT (AppT _ below) above) _) -> (below, above))
                          subsumeInstances
       ClassI _ composeInstances <- reify ''MonadCompose
       let composes = map (\(InstanceD _ (AppT (AppT _ m1) m2) _) -> (m1, m2)) composeInstances
       res <- evalStateT (makeMonadRepr' t1 t2 e) (IGS subsumes composes)
       -- runIO $ mapM (putStrLn . pprint) res
       return res


makeMonadRepr' :: Type -> Type -> Exp -> IGState Q [Dec]
makeMonadRepr' t1 t2 e
  = do reflexiveSubs <- sequence [ generateSubsume t1 t1 (\_ -> VarE 'id)
                                 , generateSubsume t2 t2 (\_ -> VarE 'id) 
                                 , generateCompose t1 t1 t1 (\_ -> VarE 'id) (\_ -> VarE 'id)
                                 , generateCompose t2 t2 t2 (\_ -> VarE 'id) (\_ -> VarE 'id)
                                 ]
       
       (_      , belowM1) <- collectedSubsumes t1
       (aboveM2, belowM2) <- collectedSubsumes t2
       subs <- sequence [ generateSubsume bm am (\x -> liftMSCasted t2 am x @.@ e @.@ liftMSCasted bm t1 x) 
                          | Below bm <- belowM1, Above am <- aboveM2 ]
       compBelows  <- sequence [ generateComposes bm1 bm2 t2 (\x -> e @.@ liftMSCasted bm1 t1 x) 
                                                             (\x -> liftMSCasted bm2 t2 x) 
                                  | Below bm1 <- belowM1, Below bm2 <- belowM2 ]
       compThrough <- sequence [ generateComposes bm1 am2 am2 (\x -> liftMSCasted t2 am2 x @.@ e @.@ liftMSCasted bm1 t1 x) 
                                                              (\_ -> VarE 'id) 
                                  | Below bm1 <- belowM1, Above am2 <- aboveM2 ]
       return ((catMaybes $ reflexiveSubs ++ subs) ++ concat (compBelows ++ compThrough))

newtype Above = Above Type deriving (Show)
newtype Below = Below Type deriving (Show)
       
collectedSubsumes :: Type -> IGState Q ([Above], [Below])
collectedSubsumes t
  = gets subsumeInsts >>= return . foldl collect ([],[])
  where collect (above,below) (tb,ta) 
          = ( if t == tb then Above ta : above else above
            , if t == ta then Below tb : below else below )
       
liftMSCasted :: Type -> Type -> Name -> Exp
liftMSCasted t1 t2 n 
  = VarE 'liftMS `SigE` (ForallT [PlainTV n] [] $ ArrowT `AppT` (t1 `AppT` VarT n) `AppT` (t2 `AppT` VarT n))
       
(@.@) :: Exp -> Exp -> Exp
a @.@ b = InfixE (Just a) (VarE (mkName ".")) (Just b)
     
generateComposes :: Type -> Type -> Type -> (Name -> Exp) -> (Name -> Exp) -> IGState Q [Dec]
generateComposes t1 t2 t3 e1 e2 = do c1 <- generateCompose t1 t2 t3 e1 e2
                                     c2 <- generateCompose t2 t1 t3 e2 e1
                                     return $ catMaybes [c1,c2]
     
generateCompose :: Type -> Type -> Type -> (Name -> Exp) -> (Name -> Exp) -> IGState Q (Maybe Dec)
generateCompose m1 m2 m3 e1 e2
  = do composes <- gets composeInsts
       if not ((m1,m2) `elem` composes) then
         do dataName <- lift $ newName ("ComposePhantom_" ++ filter isAlphaNum (show m1) 
                                                   ++ "_" ++ filter isAlphaNum (show m2))
            modify $ \st -> st { composeInsts = (m1,m2) : composeInsts st }
            x <- lift (newName "x")
            return $ Just $ 
              InstanceD [] ((ConT ''MonadCompose) `AppT` m1 `AppT` m2)
                        [ generateTypeSynonym
                        , DataInstD [] ''ComposePhantom [m1,m2] [NormalC dataName []] []
                        , ValD (VarP 'newComposePhantom) (NormalB (ConE dataName)) []
                        , FunD 'liftMC1 [Clause [WildP] (NormalB (e1 x)) []]
                        , FunD 'liftMC2 [Clause [WildP] (NormalB (e2 x)) []]
                        ]
       else return Nothing
    where 
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
    generateTypeSynonym = TySynInstD ''ResultMonad (TySynEqn [m1, m2] m3)
#else
    generateTypeSynonym = TySynInstD ''ResultMonad [m1, m2] m3
#endif     
     
generateSubsume :: Type -> Type -> (Name -> Exp) -> IGState Q (Maybe Dec)
generateSubsume m1 m2 e
  = do subsumes <- gets subsumeInsts
       if isNothing (find (== (m1,m2)) subsumes) then 
         do modify $ \st -> st { subsumeInsts = (m1,m2) : subsumeInsts st }
            x <- lift (newName "x")
            return $ Just $ 
              InstanceD [] ((ConT ''MonadSubsume) `AppT` m1 `AppT` m2)
                        [ FunD 'liftMS [Clause [] (NormalB (e x)) []] ]
       else return Nothing


