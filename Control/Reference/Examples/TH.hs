{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, FlexibleContexts #-}

-- | An example module that adds references for Template Haskell.
-- These references are used to create the TH functions that generate references.
module Control.Reference.Examples.TH where

import Control.Reference.InternalInterface

import Control.Applicative
import Language.Haskell.TH

-- | Reference all type variables inside a type
typeVariables :: Simple Traversal Type Name
typeVariables = fromTraversal freeTypeVariables'
  where freeTypeVariables' f (ForallT vars ctx t) = ForallT vars ctx <$> freeTypeVariables' f t
        freeTypeVariables' f (AppT t1 t2) = AppT <$> freeTypeVariables' f t1 <*> freeTypeVariables' f t2
        freeTypeVariables' f (SigT t k) = SigT <$> freeTypeVariables' f t <*> pure k
        freeTypeVariables' f (VarT n) = VarT <$> f n
        freeTypeVariables' _ t = pure t
 
-- | Reference the name of the type variable inside a type variable binder
typeVarName :: Simple Lens TyVarBndr Name
typeVarName = lens (\case PlainTV n -> n; KindedTV n _ -> n) 
                   (\n' -> \case PlainTV _ -> PlainTV n'; KindedTV _ k -> KindedTV n' k)

-- | Reference the characters of the name.
-- If changed there is no guarantee that the created name will be unique.
nameBaseStr :: Simple Lens Name String
nameBaseStr = iso nameBase mkName

-- | Reference the record fields in a constructor.
recFields :: Simple Partial Con [(Name, Strict, Type)]
recFields = partial (\case (RecC name flds) -> Right (flds, \flds' -> RecC name flds')
                           c -> Left c)

-- | Reference all fields (data members) in a constructor.
conFields :: Simple Lens Con [(Strict, Type)]
conFields = lens getFlds setFlds
  where getFlds (NormalC _ flds) = flds	
        getFlds (RecC _ flds) = map (\(_,a,b) -> (a,b)) flds
        getFlds (InfixC flds1 _ flds2) = [flds1, flds2]
        getFlds (ForallC _ _ c) = getFlds c
        
        setFlds flds' (NormalC n _) = NormalC n flds'
        setFlds flds' (RecC n flds) = RecC n (zipWith (\(n,_,_) (s,t) -> (n,s,t)) flds flds')
        setFlds [fld1',fld2'] (InfixC _ n _) = InfixC fld1' n fld2'
        setFlds flds' (ForallC bind ctx c) = ForallC bind ctx (setFlds flds' c)

-- | Reference the name of the constructor
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

-- | Access a function application as a list of expressions with the function application
-- at the head of the list and the arguments on it's tail.
funApplication :: Simple Lens Exp [Exp]
funApplication = lens (unfoldExpr []) (\ls _ -> foldl1 AppE ls)
  where unfoldExpr ls (AppE l r) = unfoldExpr (r : ls) l
        unfoldExpr ls e = e : ls 


