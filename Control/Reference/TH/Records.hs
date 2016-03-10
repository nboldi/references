{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

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
import Data.Function (on)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.State

import Control.Instances.Morph
import Control.Reference.InternalInterface
import Control.Reference.Examples.TH
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
           DataD _ tyConName args cons _ -> 
              createReferences tyConName (args ^? traversal&typeVarName) cons
           _ -> fail "makeReferences: Unsupported data type"
         _ -> fail "makeReferences: Expected the name of a data type or newtype"
           
createReferences :: Name -> [Name] -> [Con] -> Q [Dec]
createReferences tyConName args cons
  = let toGenerate = map getConFlds cons
        -- only those type vars are mutable that appear in at most one field of the constructor
        tvars = map (foldl (\a (_,t,_) -> foldl (flip delete) a (t ^? typeVariableNames :: [Name])) (args++args)) toGenerate
        toGenTVars = zipWith (\tg tvs -> map (\(n,t,c) -> (n,t,tvs)) tg) toGenerate tvars
        -- those references will be complete that are generated from fields that are present in every constructor
        (complete, partials) 
          = partition ((length cons ==) . length) 
              $ groupBy ((==) `on` fst3) $ sortBy (compare `on` fst3) $ concat toGenTVars
    in do comps <- mapM (createLensForField tyConName args . head) complete 
          parts <- mapM (createPartialLensForField tyConName args cons . head) partials 
          return $ concat (comps ++ parts)
  where getConFlds con@(RecC conName conFields) = map (\(n,_,t) -> (n, t, con)) conFields
        getConFlds _                            = []
        fst3 (n,_,_) = n
           
createLensForField :: Name -> [Name] -> (Name,Type,[Name]) -> Q [Dec]
createLensForField typName typArgs (fldName,fldTyp,mutArgs) 
  = do lTyp <- referenceType (ConT ''Lens) typName typArgs mutArgs fldTyp  
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
                           `AppE` LamE [VarP setVar, VarP origVar] 
                                       (RecUpdE (VarE origVar) [(fldName,VarE setVar)])
            
createPartialLensForField :: Name -> [Name] -> [Con] -> (Name,Type,[Name]) -> Q [Dec]
createPartialLensForField typName typArgs cons (fldName,fldTyp,mutArgs)
  = do lTyp <- referenceType (ConT ''Partial) typName typArgs mutArgs fldTyp  
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
                                                 .= VarE setVar $ rebuild)
                                      ]
                return $ Match bind (NormalB bindRight) []
                         
         matchWithoutField :: Con -> Q Match
         matchWithoutField con 
           = do (bind, rebuild, _) <- bindAndRebuild con
                return $ Match bind (NormalB (ConE 'Left `AppE` rebuild)) []              

-- | Creates the type of the reference being defined
referenceType :: Type -> Name -> [Name] -> [Name] -> Type -> Q Type
referenceType refType name args mutArgs fldTyp 
  = do (fldTyp',mapping) <- makePoly mutArgs fldTyp
       let args' = traversal .- (\a -> fromMaybe a (mapping ^? element a)) $ args
       return $ ForallT (map PlainTV (sort (nub (M.elems mapping ++ args)))) [] 
                        (refType `AppT` addTypeArgs name args 
                                 `AppT` addTypeArgs name args' 
                                 `AppT` fldTyp 
                                 `AppT` fldTyp') 
           
-- | Creates a new field type with changing the type variables that are bound outside
makePoly :: [Name] -> Type -> Q (Type, M.Map Name Name)
makePoly typArgs fldTyp 
  = runStateT (typVarsBounded !~ updateName $ fldTyp) M.empty           
  where typVarsBounded :: Simple Traversal Type Name
        typVarsBounded = typeVariableNames & filtered (`elem` typArgs)
        updateName name = do name' <- lift (newName (nameBase name ++ "'")) 
                             modify (M.insert name name')
                             return name'
                             
        
                             

-- | Dictates what reference names should be generated from field names
refName :: Name -> Name
refName = nameBaseStr .- \case '_':xs -> xs; xs -> '_':xs

-- * Helper functions 

hasField :: Name -> Con -> Bool
hasField n c = not $ null (c ^? recFields & traversal & _1 & filtered (==n) :: [Name])
         
fieldIndex :: Name -> Con -> Maybe Int
fieldIndex n con = (con ^? recFields) >>= findIndex (\f -> (f ^. _1) == n)
         
-- | Creates a type from applying binded type variables to a type function
addTypeArgs :: Name -> [Name] -> Type
addTypeArgs n = foldl AppT (ConT n) . map VarT
 
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
              , (ConE name : map VarE bindVars) ^. turn funApplication
              , bindVars
              )

instance Morph (StateT s m) (StateT s m) where
  morph = id
