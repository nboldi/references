{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-- | An example module that adds references for Template Haskell
-- These references are used to create the TH functions that generate
-- references.
-- Because of that it does not import 'Control.Reference' frontend module.
module Control.Reference.Examples.TH where

import Language.Haskell.TH

import Control.Reference.Representation
import Control.Reference.Predefined

import Control.Applicative

-- | Reference all type variables inside a type
typeVariables :: (Applicative w, Monad w) => Traversal' w Type Type Name Name
typeVariables = fromTraversal freeTypeVariables' freeTypeVariables'
  where freeTypeVariables' f (ForallT vars ctx t) = ForallT vars ctx <$> freeTypeVariables' f t
        freeTypeVariables' f (AppT t1 t2) = AppT <$> freeTypeVariables' f t1 <*> freeTypeVariables' f t2
        freeTypeVariables' f (SigT t k) = SigT <$> freeTypeVariables' f t <*> pure k
        freeTypeVariables' f (VarT n) = VarT <$> f n
        freeTypeVariables' _ t = pure t
 
typeVariables' :: Simple Traversal Type Name
typeVariables' = typeVariables
 
-- | Reference the name of the type variable inside a type variable binder
typeVarName :: (Applicative w, Monad w) => Lens' w TyVarBndr TyVarBndr Name Name
typeVarName = lens (\case PlainTV n -> n; KindedTV n _ -> n) 
                   (\n' -> \case PlainTV _ -> PlainTV n'; KindedTV _ k -> KindedTV n' k)

typeVarName' :: Simple Lens TyVarBndr Name
typeVarName' = typeVarName

-- | Reference the characters of the name.
-- If changed there is no guarantee that the created name will be unique.
nameBaseStr :: Monad w => Lens' w Name Name String String
nameBaseStr = iso nameBase mkName

nameBaseStr' :: Simple Lens Name String
nameBaseStr' = nameBaseStr

recFields :: Monad w => Simple' w LensPart' Con [(Name, Strict, Type)]
recFields = partial (\case (RecC _ flds) -> Just flds; _ -> Nothing) 
                    (\flds' -> \case (RecC name _) -> RecC name flds'; con -> con)

recFields' :: Simple LensPart Con [(Name, Strict, Type)]
recFields' = recFields

conFields :: Monad w => Simple' w Lens' Con [(Strict, Type)]
conFields = lens getFlds setFlds
  where getFlds (NormalC _ flds) = flds	
        getFlds (RecC _ flds) = map (\(_,a,b) -> (a,b)) flds
        getFlds (InfixC flds1 _ flds2) = [flds1, flds2]
        getFlds (ForallC _ _ c) = getFlds c
        
        setFlds flds' (NormalC n _) = NormalC n flds'
        setFlds flds' (RecC n flds) = RecC n (zipWith (\(n,_,_) (s,t) -> (n,s,t)) flds flds')
        setFlds [fld1',fld2'] (InfixC _ n _) = InfixC fld1' n fld2'
        setFlds flds' (ForallC bind ctx c) = ForallC bind ctx (setFlds flds' c)

conFields' :: Simple Lens Con [(Strict, Type)]
conFields' = conFields

conName :: Simple Lens Con Name
conName = lens getName setName
  where getName (NormalC n _)   = n	
        getName (RecC n _)      = n
        getName (InfixC _ n _)  = n
        getName (ForallC _ _ c) = getName c
        
        setName n' (NormalC _ flds)     = NormalC n' flds
        setName n' (RecC _ flds)        = RecC n' flds
        setName n' (InfixC fld1 _ fld2) = InfixC fld1 n' fld2
        setName n' (ForallC bind ctx c) = ForallC bind ctx (setName n' c)

funApplication :: Monad w => Simple' w Lens' Exp [Exp]
funApplication = lens (unfoldExpr []) (\ls _ -> foldl1 AppE ls)
  where unfoldExpr ls (AppE l r) = unfoldExpr (r : ls) l
        unfoldExpr ls e = e : ls 
        
funApplication' :: Simple Lens Exp [Exp]
funApplication' = funApplication

