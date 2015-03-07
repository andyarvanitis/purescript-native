-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Desugar
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The AST -> CoreFn desugaring step
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Data.Function (on)
import Data.List (sort, sortBy, nub)
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)
import qualified Data.Map as M

import Control.Arrow (second, (***))

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Traversals
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Sugar.TypeClasses (typeClassMemberName, superClassDictionaryNames)
import Language.PureScript.Types
import Language.PureScript.Comments
import qualified Language.PureScript.AST as A

import Debug.Trace

-- |
-- Desugars a module from AST to CoreFn representation.
--
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ Nothing) =
  error "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module coms mn decls (Just exps)) =
  let imports = nub $ mapMaybe importToCoreFn decls ++ findQualModules decls
      exps' = nub $ concatMap exportToCoreFn exps
      externs = nub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn Nothing []) decls
  in Module coms mn imports exps' externs decls'

  where

  -- |
  -- Desugars member declarations from AST to CoreFn representation.
  --
  declToCoreFn :: Maybe SourceSpan -> [Comment] -> A.Declaration -> [Bind Ann]
  declToCoreFn ss com (A.DataDeclaration Newtype a b ctors) = -- TODO: optim disabled for now
    declToCoreFn ss com (A.DataDeclaration Data a b ctors)
  declToCoreFn ss com (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
    [NonRec (properToIdent ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
  declToCoreFn _ _ d@(A.DataDeclaration Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn ss com (A.DataDeclaration Data tyName parms ctors) =
    flip map ctors $ \(ctor, _) ->
      let (_, _, ty, fields) = lookupConstructor env (Qualified (Just mn) ctor)
      in NonRec (properToIdent ctor) $ Constructor (ss, com, Just ty, Nothing) annotTyName ctor fields
    where
    annotTyName = ProperName $ (runProperName tyName) ++ ('@' : asTemplate)
    asTemplate = show $ map (cap . fst) parms
    cap (c:cs) = toUpper c : cs
  declToCoreFn ss _   (A.DataBindingGroupDeclaration ds) = concatMap (declToCoreFn ss []) ds
  declToCoreFn ss com (A.ValueDeclaration name _ _ (Right e)) =
    [NonRec name (exprToCoreFn ss com Nothing e)]
  declToCoreFn ss _   (A.BindingGroupDeclaration ds) =
    [Rec $ map (\(name, _, e) -> (name, exprToCoreFn ss [] Nothing e)) ds]
  declToCoreFn ss com (A.TypeClassDeclaration name ps supers members) =
    [NonRec (properToIdent name) $ mkTypeClassConstructor ss com (runProperName name) ps supers members]
  declToCoreFn _  com (A.PositionedDeclaration ss com1 d) =
    declToCoreFn (Just ss) (com ++ com1) d
  declToCoreFn _ _ _ = []

  -- |
  -- Desugars expressions from AST to CoreFn representation.
  --
  exprToCoreFn :: Maybe SourceSpan -> [Comment] -> Maybe Type -> A.Expr -> Expr Ann
  exprToCoreFn ss com ty (A.NumericLiteral v) =
    Literal (ss, com, ty, Nothing) (NumericLiteral v)
  exprToCoreFn ss com ty (A.StringLiteral v) =
    Literal (ss, com, ty, Nothing) (StringLiteral v)
  exprToCoreFn ss com ty (A.BooleanLiteral v) =
    Literal (ss, com, ty, Nothing) (BooleanLiteral v)
  exprToCoreFn ss com ty (A.ArrayLiteral vs) =
    Literal (ss, com, ty, Nothing) (ArrayLiteral $ map (exprToCoreFn ss [] Nothing) vs)
  exprToCoreFn ss com ty (A.ObjectLiteral vs) =
    Literal (ss, com, ty, Nothing) (ObjectLiteral $ map (second (exprToCoreFn ss [] Nothing)) vs)
  exprToCoreFn ss com ty (A.Accessor name v) =
    Accessor (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing obj) $ map (second (exprToCoreFn ss [] Nothing)) vs
  exprToCoreFn ss com ty (A.Abs (Left name) v) =
    Abs (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (A.Abs _ _) =
    error "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com ty (A.App v1 v2) =
    App (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing v1) (exprToCoreFn ss [] Nothing v2)
  exprToCoreFn ss com Nothing (A.Var ident@(Qualified (Just mn) name))
    | Just (ty, TypeClassAccessorImport, _) <- M.lookup (mn, name) (names env) = Var (ss, com, Just ty, Nothing) ident
  exprToCoreFn ss com ty (A.Var ident) =
    Var (ss, com, ty, Nothing) ident
  exprToCoreFn ss com ty (A.IfThenElse v1 v2 v3) =
    Case (ss, com, ty, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True]
                        (Right $ exprToCoreFn Nothing [] Nothing v2)
      , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False]
                        (Right $ exprToCoreFn Nothing [] Nothing v3) ]
  exprToCoreFn ss com ty (A.Constructor name) =
    Var (ss, com, ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com ty (A.Case vs alts) =
    Case (ss, com, ty, Nothing) (map (exprToCoreFn ss [] Nothing) vs) (map (altToCoreFn ss) alts)
  -- exprToCoreFn ss com ty@(Just _) (A.TypedValue _ v@(A.Constructor name) _) =
  --   exprToCoreFn ss com ty v
  exprToCoreFn ss com _ (A.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com ty (A.Let ds v) =
    Let (ss, com, ty, Nothing) (concatMap (declToCoreFn ss []) ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com (Just ty) (A.TypeClassDictionaryConstructorApp name@(Qualified mn (ProperName cname)) c)
    | Just (parms, _, _) <- M.lookup name (typeClasses env) =
    exprToCoreFn ss com (Just ty) (A.TypeClassDictionaryConstructorApp (annotName parms) c)
    where
      annotName parms = Qualified mn (ProperName (cname ++ '@' : (show $ map fst parms)))
  exprToCoreFn ss com ty (A.TypeClassDictionaryConstructorApp name (A.TypedValue _ (A.ObjectLiteral vs) _)) =
    let args = map (exprToCoreFn ss [] Nothing . snd) $ sortBy (compare `on` fst) vs
        ctor = Var (ss, [], rowType, Just IsTypeClassConstructor) (fmap properToIdent name)
    in foldl (App (ss, com, ty, Nothing)) ctor args
    where
      rowType
        | Qualified m (ProperName pname) <- name,
          Just (_, tys, _) <- M.lookup (Qualified m (ProperName $ takeWhile (/='@') pname)) (typeClasses env)
        = Just $ rowFromList ((sortBy (compare `on` fst) ((map (\(i,t) -> (rmParens $ runIdent i,t)) tys))), REmpty)
        -- TODO: remove after testing
        | otherwise = error $ show $ map fst vs
      rmParens ('(':ss) = init ss
      rmParens s = s

  exprToCoreFn ss com ty  (A.TypeClassDictionaryAccessor _ ident) =
    Abs (ss, com, ty, Nothing) (Ident "dict")
      (Accessor nullAnn (runIdent ident) (Var nullAnn $ Qualified Nothing (Ident "dict")))
  exprToCoreFn _ com ty (A.PositionedValue ss com1 v) =
    exprToCoreFn (Just ss) (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- |
  -- Desugars case alternatives from AST to CoreFn representation.
  --
  altToCoreFn :: Maybe SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go (Left ges) = Left $ map (exprToCoreFn ss [] Nothing *** exprToCoreFn ss [] Nothing) ges
    go (Right e) = Right (exprToCoreFn ss [] Nothing e)

  -- |
  -- Desugars case binders from AST to CoreFn representation.
  --
  binderToCoreFn :: Maybe SourceSpan -> [Comment] -> A.Binder -> Binder Ann
  binderToCoreFn ss com (A.NullBinder) =
    NullBinder (ss, com, Nothing, Nothing)
  binderToCoreFn ss com (A.BooleanBinder b) =
    LiteralBinder (ss, com, Nothing, Nothing) (BooleanLiteral b)
  binderToCoreFn ss com (A.StringBinder s) =
    LiteralBinder (ss, com, Nothing, Nothing) (StringLiteral s)
  binderToCoreFn ss com (A.NumberBinder n) =
    LiteralBinder (ss, com, Nothing, Nothing) (NumericLiteral n)
  binderToCoreFn ss com (A.VarBinder name) =
    VarBinder (ss, com, Nothing, Nothing) name
  binderToCoreFn ss com (A.ConstructorBinder dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (map (binderToCoreFn ss []) bs)
  binderToCoreFn ss com (A.ObjectBinder bs) =
    LiteralBinder (ss, com, Nothing, Nothing) (ObjectLiteral $ map (second (binderToCoreFn ss [])) bs)
  binderToCoreFn ss com (A.ArrayBinder bs) =
    LiteralBinder (ss, com, Nothing, Nothing) (ArrayLiteral $ map (binderToCoreFn ss []) bs)
  binderToCoreFn ss com (A.ConsBinder b1 b2) =
    let arrCtor = Qualified (Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")
    in ConstructorBinder (ss, com, Nothing, Nothing) arrCtor arrCtor $ map (binderToCoreFn ss []) [b1, b2]
  binderToCoreFn ss com (A.NamedBinder name b) =
    NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn (Just ss) (com ++ com1) b

  -- |
  -- Gets metadata for data constructors.
  --
  getConstructorMeta :: Qualified ProperName -> Meta
  getConstructorMeta ctor =
    case lookupConstructor env ctor of
      (Newtype, _, _, _) -> IsNewtype
      dc@(Data, _, _, fields) ->
        let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
        in IsConstructor constructorType fields
    where
    numConstructors :: (Qualified ProperName, (DataDeclType, ProperName, Type, [Ident])) -> Int
    numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env
    typeConstructor :: (Qualified ProperName, (DataDeclType, ProperName, Type, [Ident])) -> (ModuleName, ProperName)
    typeConstructor (Qualified (Just mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
    typeConstructor _ = error "Invalid argument to typeConstructor"

-- |
-- Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
--
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) (const []) fqValues fqBinders (const []) (const [])
  in f `concatMap` decls
  where
  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var (Qualified (Just mn) _)) = [mn]
  fqValues (A.Constructor (Qualified (Just mn) _)) = [mn]
  fqValues _ = []
  
  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder (Qualified (Just mn) _) _) = [mn]
  fqBinders _ = []

-- |
-- Desugars import declarations from AST to CoreFn representation.
--
importToCoreFn :: A.Declaration -> Maybe ModuleName
importToCoreFn (A.ImportDeclaration name _ _) = Just name
importToCoreFn (A.PositionedDeclaration _ _ d) = importToCoreFn d
importToCoreFn _ = Nothing

-- |
-- Desugars foreign declarations from AST to CoreFn representation.
--
externToCoreFn :: A.Declaration -> Maybe ForeignDecl
externToCoreFn (A.ExternDeclaration _ name js ty) = Just (name, js, ty)
externToCoreFn (A.ExternInstanceDeclaration name _ _ _) = Just (name, Nothing, tyObject)
externToCoreFn (A.PositionedDeclaration _ _ d) = externToCoreFn d
externToCoreFn _ = Nothing

-- |
-- Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, class
-- constructor, instances and values are flattened into one list.
--
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ (Just dctors)) = map properToIdent dctors
exportToCoreFn (A.ValueRef name) = [name]
exportToCoreFn (A.TypeClassRef name) = [properToIdent name]
exportToCoreFn (A.TypeInstanceRef name) = [name]
exportToCoreFn (A.PositionedDeclarationRef _ _ d) = exportToCoreFn d
exportToCoreFn _ = []

-- |
-- Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
--
mkTypeClassConstructor :: Maybe SourceSpan -> [Comment] -> String -> [(String, a)] -> [Constraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor ss com _ _ [] [] = Literal (ss, com, Nothing, Just IsTypeClassConstructor) (ObjectLiteral [])
mkTypeClassConstructor ss com name parms supers members =
  let args@(a:as) = sort $ map typeClassMemberName members ++ superClassDictionaryNames supers
      as' = sortBy (compare `on` fst) $ map (\m -> (typeClassMemberName m, typeClassMemberType m)) members
                                     ++ map (\m -> (m, Nothing)) (superClassDictionaryNames supers)
      props = [ (arg, Var nullAnn $ Qualified Nothing (Ident arg)) | arg <- args ]
      dict = Literal nullAnn (ObjectLiteral props)
  in Abs (ss, com, Nothing, Just IsTypeClassConstructor)
         (Ident $ name ++ '@' : (show $ map fst parms))
         (foldr mkAbs dict as')
  where
    typeClassMemberType :: A.Declaration -> Maybe Type
    typeClassMemberType (A.TypeDeclaration _ ty) = Just ty
    typeClassMemberType (A.PositionedDeclaration _ _ d) = typeClassMemberType d
    typeClassMemberType d = error $ "Invalid declaration in type class definition: " ++ show d

    mkAbs :: (String, Maybe Type) -> Expr Ann -> Expr Ann
    mkAbs (nm, ty) = Abs (Nothing, [], ty, Nothing) (Ident nm)

-- |
-- Converts a ProperName to an Ident.
--
properToIdent :: ProperName -> Ident
properToIdent = Ident . runProperName
