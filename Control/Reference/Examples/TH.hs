{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, FlexibleContexts #-}

-- | An example module that adds references for Template Haskell.
-- These references are used to create the TH functions that generate references.
module Control.Reference.Examples.TH where

import Control.Reference.InternalInterface
import Control.Reference.TupleInstances

import Control.Applicative
import Language.Haskell.TH

-- | Reference all type variables inside a type
typeVariableNames :: Simple Traversal Type Name
typeVariableNames = typeVariables & typeVar
        
-- | Reference the name of the type variable
typeVar :: Simple Partial Type Name
typeVar = partial ( \case VarT n -> Right (n, \n' -> VarT n')
                          other -> Left other )
        
-- | Reference all type variables inside a type
typeVariables :: Simple Traversal Type Type
typeVariables = fromTraversal typeVariables'
  where typeVariables' f (ForallT vars ctx t) = ForallT vars ctx <$> typeVariables' f t
        typeVariables' f (AppT t1 t2) = AppT <$> typeVariables' f t1 <*> typeVariables' f t2
        typeVariables' f (SigT t k) = SigT <$> typeVariables' f t <*> pure k
        typeVariables' f tv@(VarT _) = f tv
        typeVariables' _ t = pure t        
        
-- | Reference all type variables not binded by a forall
freeTypeVariables :: Simple Traversal Type Type
freeTypeVariables = fromTraversal (freeTypeVariables' [])
  where freeTypeVariables' bn f (ForallT vars ctx t) 
          = ForallT vars ctx <$> freeTypeVariables' (bn ++ (vars ^* traverse&typeVarName)) f t
        freeTypeVariables' bn f (AppT t1 t2) = AppT <$> freeTypeVariables' bn f t1 <*> freeTypeVariables' bn f t2
        freeTypeVariables' bn f (SigT t k) = SigT <$> freeTypeVariables' bn f t <*> pure k
        freeTypeVariables' bn f tv@(VarT n) = if n `elem` bn then pure tv else f tv
        freeTypeVariables' bn _ t = pure t
 
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

-- | Reference types of fields
conTypes :: Simple Traversal Con Type
conTypes = conFields & traverse & _2
        
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
funApplication :: Simple Iso Exp [Exp]
funApplication = iso (unfoldExpr []) (foldl1 AppE)
  where unfoldExpr ls (AppE l r) = unfoldExpr (r : ls) l
        unfoldExpr ls e = e : ls 

-- | Accesses the name of the defined object. Does not return name in signatures.
definedName :: Simple Partial Dec Name
definedName
  = partial (\case FunD n c                 -> Right (n, \n' -> FunD n' c)
                   ValD (VarP n) b w        -> Right (n, \n' -> ValD (VarP n') b w) 
                   DataD c n tv con d       -> Right (n, \n' -> DataD c n' tv con d) 
                   NewtypeD c n tv con d    -> Right (n, \n' -> NewtypeD c n' tv con d) 
                   TySynD n tv t            -> Right (n, \n' -> TySynD n' tv t) 
                   ClassD c n tv fd f       -> Right (n, \n' -> ClassD c n' tv fd f) 
                   FamilyD fl n tv k        -> Right (n, \n' -> FamilyD fl n' tv k) 
                   other -> Left other)

-- | Accesses the constructors of a data or newtype definition.
-- After changing the definition becames a newtype if there is only one constructor.
definedConstructors :: Simple Partial Dec [Con]
definedConstructors
  = partial (\case DataD c n tv con d       -> Right (con, \con' -> createConOrNewtype c n tv con' d) 
                   NewtypeD c n tv con d    -> Right ([con], \con' -> createConOrNewtype c n tv con' d) 
                   other -> Left other)
  where createConOrNewtype c n tv [con] d = NewtypeD c n tv con d
        createConOrNewtype c n tv cons d = DataD c n tv cons d
        
-- | Accesses the type variables of a definition
definedTypeArgs :: Simple Partial Dec [TyVarBndr]
definedTypeArgs
  = partial (\case DataD c n tv con d       -> Right (tv, \tv' -> DataD c n tv' con d) 
                   NewtypeD c n tv con d    -> Right (tv, \tv' -> NewtypeD c n tv' con d) 
                   TySynD n tv t            -> Right (tv, \tv' -> TySynD n tv' t) 
                   ClassD c n tv fd f       -> Right (tv, \tv' -> ClassD c n tv' fd f) 
                   FamilyD fl n tv k        -> Right (tv, \tv' -> FamilyD fl n tv' k) 
                   other -> Left other)
