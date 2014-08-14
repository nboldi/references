{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, DoAndIfThenElse, TypeOperators #-}

-- | A module for making connections between different monads.
module Control.Reference.TH.Monad
       (makeMonadRepr
       , ToQType(..)
       , ToQExp(..)
       ) where

import Control.Reference.Representation
import Control.Monad.State
import Data.List
import Data.Maybe
import Language.Haskell.TH

-- | A type name or a type expression, that can be converted
-- into a type inside 'Q'.
class ToQType t where
  toQType :: t -> Q Type

instance ToQType Type where 
  toQType = return    
instance ToQType (Q Type) where 
  toQType = id  
instance ToQType Name where 
  toQType = return . ConT  

-- | A variable or function name or an expression, that can be converted
-- into an expression inside 'Q'.
class ToQExp t where
  toQExp :: t -> Q Exp
  
instance ToQExp (Q Exp) where 
  toQExp = id  
instance ToQExp Name where 
  toQExp = return . VarE

type IGState m a = StateT InstanceGenState m a
  
data InstanceGenState = IGS { subsumeInsts :: [(Type, Type)] } deriving Show
   
-- | Creates '!<!' instances from reflectivity, and transitivity of the relation.
-- Uses data from all instances declared so far.
makeMonadRepr :: (ToQType t1, ToQType t2, ToQExp e) 
              => t1 -> t2 -> e -> Q [Dec]
makeMonadRepr m1' m2' e'
  = do t1 <- toQType m1'; t2 <- toQType m2'; e <- toQExp e' 
       ClassI _ subsumeInstances <- reify ''(!<!)
       let subsumes = map (\(InstanceD _ (AppT (AppT _ below) above) _) -> (below, above))
                          subsumeInstances
       evalStateT (makeMonadRepr' t1 t2 e) (IGS subsumes)

makeMonadRepr' :: Type -> Type -> Exp -> IGState Q [Dec]
makeMonadRepr' t1 t2 e
  = do reflexiveSubs <- sequence [ generateSubsume t1 t1 (\_ -> VarE 'id)
                                 , generateSubsume t2 t2 (\_ -> VarE 'id) 
                                 ]
       
       (_      , belowM1) <- collectedSubsumes t1
       (aboveM2, _)       <- collectedSubsumes t2
       subs <- sequence [ generateSubsume bm am (\x -> liftMSCasted t2 am x @.@ e @.@ liftMSCasted bm t1 x) 
                          | Below bm <- belowM1, Above am <- aboveM2 ]
       return (catMaybes $ reflexiveSubs ++ subs)

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
  = VarE 'morph `SigE` (ForallT [PlainTV n] [] $ ArrowT `AppT` (t1 `AppT` VarT n) `AppT` (t2 `AppT` VarT n))
       
(@.@) :: Exp -> Exp -> Exp
a @.@ b = InfixE (Just a) (VarE (mkName ".")) (Just b)
     
generateSubsume :: Type -> Type -> (Name -> Exp) -> IGState Q (Maybe Dec)
generateSubsume m1 m2 e
  = do subsumes <- gets subsumeInsts
       if isNothing (find (== (m1,m2)) subsumes) then 
         do modify $ \st -> st { subsumeInsts = (m1,m2) : subsumeInsts st }
            x <- lift (newName "x")
            return $ Just $ 
              InstanceD [] (ConT ''(!<!) `AppT` m1 `AppT` m2)
                        [ FunD 'morph [Clause [] (NormalB (e x)) []] ]
       else return Nothing


