{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
This module can be used to generate references for record fields.
If the field surely exists, a 'Lens' will be generated.
If the field may not exist, it will be a 'Partial' lens.

It will have the maximum amount of polymorphism it can create.

If the name of the field starts with "_", the name of the field will be the same with "_" removed. 
If not, the reference name will be the field name with "_" added te the start.

The following code sample:

@
data Maybe' a = Just' { _fromJust' :: a }
              | Nothing'
$(makeReferences ''Maybe)

data Tuple a b = Tuple { _fst' :: a, _snd' :: b }
$(makeReferences ''Tuple)
@

Is equivalent to:

@
data Maybe' a = Just' { _fromJust' :: a }
              | Nothing'
              
fromJust' :: 'Partial' (Maybe' a) (Maybe' b) a b
fromJust' = 'partial' (\case Just' x -> Right (x, \y -> return (Just' y))
                           Nothing' -> Left (return Nothing'))

data Tuple a b = Tuple { _fst' :: a, _snd' :: b }
fst' :: 'Lens' (Tuple a c) (Tuple b c) a b
fst' = 'lens' _fst' (\b tup -> tup { _fst' = b })
snd' :: 'Lens' (Tuple a c) (Tuple a d) c d
snd' = 'lens' _snd' (\b tup -> tup { _snd' = b })
@
-}
module Control.Reference.TH.Records (makeReferences, debugTH) where

import Language.Haskell.TH hiding (ListT)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Trans.List
import Control.Monad.Trans.State
import Control.Applicative

import Control.Reference.Representation
import Control.Reference.Predefined
import Control.Reference.Operators
import Control.Reference.Examples.TH
import Control.Reference.TH.MonadInstances
import Control.Reference.TupleInstances

-- | Shows the generated declarations instead of using them.
debugTH :: Q [Dec] -> Q [Dec]
debugTH d = d >>= runIO . putStrLn . pprint >> return []

-- | Creates references for fields of a data structure.
makeReferences :: Name -> Q [Dec]
makeReferences n 
  = do inf <- reify n
       case inf of
         TyConI decl -> case newtypeToData decl of
           DataD ctx tyConName args cons _ -> case cons of
             [con] -> makeLensesForCon tyConName args con 
             _ -> liftM concat $ mapM (makePartialLensesForCon tyConName args cons) cons
           _ -> fail "makeReferences: Unsupported data type"
         _ -> fail "makeReferences: Expected the name of a data type or newtype"
                

makeLensesForCon :: Name -> [TyVarBndr] -> Con -> Q [Dec]
makeLensesForCon tyName tyVars (RecC conName conFields) 
  = liftM concat $ mapM (\(n, _, t) -> createLensForField tyName tyVars conName n t) conFields
makeLensesForCon _ _ _ = return []
             
createLensForField :: Name -> [TyVarBndr] -> Name -> Name -> Type -> Q [Dec]
createLensForField typName typArgs conName fldName fldTyp 
  = do lTyp <- referenceType (ConT ''Lens) typName typArgs fldTyp  
       lensBody <- genLensBody
       return [ SigD lensName lTyp
              , ValD (VarP lensName) (NormalB $ lensBody) []
              ] 
   where lensName = refName fldName
   
         genLensBody :: Q Exp
         genLensBody 
           = do setVar <- newName "b"
                origVar <- newName "s"
                return $ VarE 'lens 
                           `AppE` VarE fldName 
                           `AppE` LamE [VarP setVar, AsP origVar (RecP conName [])] 
                                       (RecUpdE (VarE origVar) [(fldName,VarE setVar)])
           
           
makePartialLensesForCon :: Name -> [TyVarBndr] -> [Con] -> Con -> Q [Dec]
makePartialLensesForCon tyName tyVars cons (RecC conName conFields) 
  = liftM concat $ mapM (\(n, _, t) -> createPartialLensForField tyName tyVars conName cons n t) conFields
makePartialLensesForCon _ _ _ _ = return []
           
createPartialLensForField :: Name -> [TyVarBndr] -> Name -> [Con] -> Name -> Type -> Q [Dec]
createPartialLensForField  typName typArgs conName cons fldName fldTyp 
  = do lTyp <- referenceType (ConT ''Partial) typName typArgs fldTyp  
       lensBody <- genLensBody
       return [ SigD lensName lTyp
              , ValD (VarP lensName) (NormalB $ lensBody) []
              ] 
   where lensName = refName fldName
   
         genLensBody :: Q Exp
         genLensBody 
           = do matchesWithField <- mapM matchWithField consWithField 
                matchesWithoutField <- mapM matchWithoutField consWithoutField
                name <- newName "x"
                return $ VarE 'partial 
                           `AppE` LamE [VarP name]
                                       (CaseE (VarE name)
                                              ( matchesWithField ++ matchesWithoutField ))
                           
         (consWithField, consWithoutField) 
           = partition (hasField fldName) cons
           
         matchWithField :: Con -> Q Match
         matchWithField con 
           = do (bind, rebuild, vars) <- bindAndRebuild con
                setVar <- newName "b"
                let Just bindInd = fieldIndex fldName con
                    bindRight 
                      = ConE 'Right 
                          `AppE` TupE [ VarE (vars !! bindInd)
                                      , LamE [VarP setVar] 
                                             (funApplication & element (bindInd+1)
                                                 ?= VarE setVar $ rebuild)
                                      ]
                return $ Match bind (NormalB bindRight) []
                         
         matchWithoutField :: Con -> Q Match
         matchWithoutField con 
           = do (bind, rebuild, _) <- bindAndRebuild con
                return $ Match bind (NormalB (ConE 'Left `AppE` rebuild)) []
                                       
           
referenceType :: Type -> Name -> [TyVarBndr] -> Type -> Q Type
referenceType refType name args fldTyp 
  = do let argTypes = args ^* traverse&typeVarName
       (fldTyp',mapping) <- makePoly argTypes fldTyp
       let args' = traverse&typeVarName *- (\a -> fromMaybe a (mapping ^? element a)) $ args
       return $ ForallT (map PlainTV (sort (nub (M.elems mapping ++ argTypes)))) [] 
                        (refType `AppT` addTypeArgs name args 
                                 `AppT` addTypeArgs name args' 
                                 `AppT` fldTyp 
                                 `AppT` fldTyp') 
           
-- | Creates a new field type with changing the type variables that are bound outside
makePoly :: [Name] -> Type -> Q (Type, M.Map Name Name)
makePoly typArgs fldTyp 
  = runStateT (typVarsBounded #~ updateName $ fldTyp) M.empty           
  where typVarsBounded :: Simple (StateTraversal' (M.Map Name Name) Q) Type Name
        typVarsBounded = typeVariableNames & filtered (`elem` typArgs)
        updateName name = do name' <- lift (newName (nameBase name ++ "'")) 
                             modify (M.insert name name')
                             return name'
                             
        
                             

-- | Dictates what reference names should be generated from field names
refName :: Name -> Name
refName = nameBaseStr .- \case '_':xs -> xs; xs -> '_':xs

-- * Helper functions 

hasField :: Name -> Con -> Bool
hasField n = not . null . (^* recFields & traverse & _1 & filtered (==n))
         
fieldIndex :: Name -> Con -> Maybe Int
fieldIndex n con = (con ^? recFields) >>= findIndex (\f -> (f ^. _1) == n)
         
-- | Creates a type from applying binded type variables to a type function
addTypeArgs :: Name -> [TyVarBndr] -> Type
addTypeArgs n = foldl AppT (ConT n) 
                  . map (VarT . (^. typeVarName))
 
newtypeToData :: Dec -> Dec
newtypeToData (NewtypeD ctx name tvars con derives) 
  = DataD ctx name tvars [con] derives
newtypeToData d = d

bindAndRebuild :: Con -> Q (Pat, Exp, [Name])
bindAndRebuild con 
  = do let name = con ^. conName
           fields = con ^. conFields
       bindVars <- replicateM (length fields) (newName "fld")
       return ( ConP name (map VarP bindVars)
              , -- TODO : use funApplication isomorphisms
                foldl AppE (ConE name) (map VarE bindVars)
              , bindVars
              )


