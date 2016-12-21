---------------------------------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified C++1x intermediate representation from
-- Purescript code.
--
---------------------------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp
  ( module AST
  , module Common
  , moduleToCpp
  , OtherOptions(..)
  , P.prettyPrintCpp
  ) where

import Prelude.Compat
import Data.Char (isLetter)
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as Text

import Control.Monad (forM, replicateM)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Supply.Class
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.Cpp.AST as AST
import Language.PureScript.CodeGen.Cpp.Common as Common
import Language.PureScript.CodeGen.Cpp.File
import Language.PureScript.CodeGen.Cpp.Optimizer
import Language.PureScript.CodeGen.Cpp.Optimizer.TCO
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CodeGen.Cpp.Unicode
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Sugar.TypeClasses (superClassDictionaryNames)
import Language.PureScript.Traversals (sndM)

import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Environment as E
import qualified Language.PureScript.Pretty.Cpp as P
import qualified Language.PureScript.Types as T

data OtherOptions = OtherOptions
  { optionsUseUCNs :: Bool
  } deriving Show

-- |
-- Generate code in the simplified C++1x intermediate representation for all declarations in a
-- module.
--
---------------------------------------------------------------------------------------------------
moduleToCpp
  :: forall m.
     (Applicative m, Monad m, MonadReader Options m, MonadSupply m)
  => OtherOptions -> E.Environment -> Module Ann -> m [Cpp]
---------------------------------------------------------------------------------------------------
moduleToCpp otherOpts env (Module _ mn imps _ foreigns decls) = do
  cppImports <-
    traverse (pure . runModuleName) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $
    (snd <$> imps)
  let cppImports' = "PureScript" : cppImports
  cppDecls <- concat <$> mapM (bindToCpp [TopLevel]) decls
  let cpps = filterInlineFuncs <$> cppDecls
      namesmap =
        M.toList . M.mapKeysMonotonic (qualifiedToCpp id) . M.map (\(t, _, _) -> arity t) $
        E.names env
      datamap =
        M.toList .
        M.mapKeysMonotonic (qualifiedToCpp (Ident . runProperName)) .
        M.map (\(_, _, _, fields) -> length fields) $
        E.dataConstructors env
  cpps' <- traverse (optimize $ namesmap ++ datamap) cpps
  foreignWrappers <- curriedForeigns
  let optimized = (otherTransforms otherOpts <$> cpps') ++ foreignWrappers
  let moduleHeader =
        fileBegin mn "HH" ++
        P.linebreak ++
        ((\i -> CppInclude i i) <$> cppImports') ++
        (if not (null foreigns)
           then [CppInclude "" (runModuleName mn <> "_ffi")]
           else []) ++
        P.linebreak ++
        [ CppNamespace (runModuleName mn) $
          (CppUseNamespace <$> cppImports') ++
          P.linebreak ++ dataCtors ++ toHeader optimized ++ toHeaderFns optimized
        ] ++
        P.linebreak ++ fileEnd mn "HH"
  let bodyCpps = toBodyDecl optimized ++ toBody optimized
      symbols = allSymbols bodyCpps
      moduleBody =
        CppInclude (runModuleName mn) (runModuleName mn) :
        P.linebreak ++
        (CppDefineSymbol <$> symbols) ++
        (if null bodyCpps
           then []
           else [ CppNamespace (runModuleName mn) $
                  (CppUseNamespace <$> cppImports') ++ P.linebreak ++ bodyCpps
                ]) ++
        P.linebreak ++
        (if isMain mn
           then [nativeMain] ++ P.linebreak
           else [])
  return $ moduleHeader ++ CppEndOfHeader : moduleBody
  where
  -- |
  -- Generate code in the simplified C++1x intermediate representation for a declaration
  --
  -------------------------------------------------------------------------------------------------
  bindToCpp :: [ValueQual] -> Bind Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  bindToCpp qs (NonRec ann ident val) = return <$> declToCpp qs (ann, ident) val
  bindToCpp qs (Rec vals) = forM vals (uncurry $ declToCpp (Recursive : qs))
  -- |
  -- Desugar a declaration into a variable introduction or named function
  -- declaration.
  -------------------------------------------------------------------------------------------------
  declToCpp :: [ValueQual] -> (Ann, Ident) -> Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  declToCpp _ (_, ident) e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let className = identToCpp ident
        (supers, members) =
           span (safeName C.__superclass_ `Text.isPrefixOf`) (identToCpp <$> (fst $ unAbs e []))
    in return $ CppStruct className [CppEnum Nothing Nothing (sort supers <> members)]
  declToCpp vqs (_, ident) (Abs (_, com, ty, _) arg body) = do
    fn <-
      if TopLevel `elem` vqs
        then do
          remainingArgs <- replicateM (arity' - length args' - 1) freshName
          let remainingArgs' = zip remainingArgs (repeat aty)
          if isTypeClassMember
            then do -- typeclass member functions are stored curried
              body' <- innerLambdas . optIndexers classes <$> valueToCpp (snd abs')
              return $
                CppNamespace
                  ""
                  [ CppFunction (curriedName name) [arg'] rty [Inline] (returnBlock body')
                  , CppFunction
                      name
                      (arg' : args' ++ remainingArgs')
                      rty
                      (vqs ++
                       if isAccessor (snd abs')
                         then [Inline]
                         else [])
                      (returnBlock $
                       foldl
                         (\fn a -> CppApp fn [a])
                         body'
                         (CppVar . fst <$> (args' ++ remainingArgs')))
                  ]
            else do
              body' <-
                innerLambdas . optIndexers classes <$>
                valueToCpp
                  (if null remainingArgs
                     then snd abs'
                     else curriedApp
                            (tail remainingArgs)
                            (App
                               nullAnn
                               (snd abs')
                               (Var nullAnn . Qualified Nothing . Ident $ head remainingArgs)))
              return $
                CppNamespace
                  ""
                  [ CppFunction
                      name
                      (arg' : args' ++ remainingArgs')
                      rty
                      (vqs ++
                       if isAccessor (snd abs')
                         then [Inline]
                         else [])
                      (returnBlock body')
                  , CppFunction
                      (curriedName name)
                      [arg']
                      rty
                      (if arity' == 1
                         then [Inline]
                         else [])
                      (returnBlock $
                       curriedLambda
                         (fst <$> args' ++ remainingArgs')
                         (CppApp
                           (CppVar name)
                           (CppVar . fst <$> (arg' : args' ++ remainingArgs'))))
                  ]
        else do
          body' <- innerLambdas . optIndexers classes <$> valueToCpp body
          return $
            CppFunction
              name
              [arg']
              rty
              (vqs ++
               if isGet body'
                 then [Inline]
                 else [])
              (returnBlock body')
    return (CppComment com $ convertIfPrimFn ty' fn)
    where
    ty'
      | Just t <- ty = Just t
      | Just (t, _, _) <- M.lookup (Qualified (Just mn) ident) (E.names env) = Just t
      | otherwise = Nothing
    isTypeClassMember = ident `elem` typeClassMembers
    arity' = maybe 0 arity ty'
    name = identToCpp ident
    aty = Just $ Any [Const, Ref]
    rty = Just $ Any []
    arg' = (identToCpp arg, aty)
    args' = (\i -> (identToCpp i, aty)) <$> fst abs'
    abs' = unAbs body []
    isAccessor :: Expr Ann -> Bool
    isAccessor Accessor {} = True
    isAccessor _ = False
    isGet :: Cpp -> Bool
    isGet (CppMapGet (CppSymbol _) (CppVar _)) = True
    isGet _ = False
    classes =
      zip
        (CppVar . fst <$> arg' : args')
        (qualifiedToCpp (Ident . runProperName) . T.constraintClass <$>
         maybe [] extractConstraints ty')
  declToCpp _ (_, ident) (Constructor _ _ (ProperName _) []) =
    return $
    CppFunction
      (identToCpp ident)
      []
      (Just $ Any [])
      [Inline]
      (returnBlock $ CppDataLiteral [CppAccessor (CppVar $ identToCpp ident) (CppVar ctorNS)])
  declToCpp _ (_, ident) (Constructor _ _ (ProperName _) fields) =
    return . CppNamespace "" $
    [ CppFunction
        name
        (farg <$> fields')
        (Just $ Any [])
        [Inline]
        (returnBlock $
         CppDataLiteral $ CppAccessor (CppVar name) (CppVar ctorNS) : (CppVar <$> fields'))
    ] ++
    if length fields > 0
      then [ CppFunction
               (curriedName name)
               (farg <$> f)
               (Just $ Any [])
               (if length fields == 1
                  then [Inline]
                  else [])
               (CppBlock (fieldLambdas fs))
           ]
      else []
    where
    name = identToCpp ident
    fields' = identToCpp <$> fields
    (f, fs)
      | (f':fs') <- fields' = ([f'], fs')
      | otherwise = ([], [])
    farg = \x -> (x, Just $ Any [Const, Ref])
    fieldLambdas flds
      | (f':fs') <- flds =
        [ CppReturn . maybeRemCaps $
          CppLambda [CaptureAll] (farg <$> [f']) (Just $ Any []) (CppBlock $ fieldLambdas fs')
        ]
      | otherwise = [CppReturn $ CppApp (CppVar name) (CppVar <$> fields')]
  declToCpp qs (_, ident) e
    | Just ty <- exprType e
    , TopLevel `elem` qs
    , arity' <- arity ty
    , arity' > 0 = do
      argNames <- replicateM arity' freshName
      body' <-
        valueToCpp $
        curriedApp
          (tail argNames)
          (App nullAnn e (Var nullAnn . Qualified Nothing . Ident $ head argNames))
      let fn' =
            CppFunction
              (curriedName name)
              [(head argNames, Just $ Any [Const, Ref])]
              rty
              []
              (returnBlock $
               curriedLambda (tail argNames) (CppApp (CppVar name) (CppVar <$> argNames)))
      return $
        CppNamespace "" $
        [CppFunction name (zip argNames $ repeat aty) rty [] (returnBlock body')] ++
        if arity' > 0
          then [fn']
          else []
    where
    name = identToCpp ident
    aty = Just $ Any [Const, Ref]
    rty = Just $ Any []
  declToCpp qs (_, ident) val = do
    val' <- valueToCpp val
    return $ CppVariableIntroduction (identToCpp ident, Just $ Any [Const]) qs (Just val')
  -------------------------------------------------------------------------------------------------
  exprType :: Expr Ann -> Maybe T.Type
  -------------------------------------------------------------------------------------------------
  exprType (Var (_, _, Just ty, _) _) = Just ty
  exprType (App (_, _, Just ty, _) _ _) = Just ty
  exprType (Case (_, _, Just ty, _) _ _) = Just ty
  exprType (Let (_, _, Just ty, _) _ _) = Just ty
  exprType _ = Nothing
  -------------------------------------------------------------------------------------------------
  fnToLambda :: Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  fnToLambda (CppFunction name args _ qs body) =
    CppVariableIntroduction
      (name, Just $ Any [Const])
      (filter (== Static) qs)
      (Just $ CppLambda cs args (Just $ Any []) body)
    where
    cs
      | (not . null) (qs `intersect` [Inline, Static]) = []
      | otherwise = [CaptureAll]
  fnToLambda (CppComment com cpp) = CppComment com (fnToLambda cpp)
  fnToLambda _ = error "Not a function!"
  -------------------------------------------------------------------------------------------------
  innerLambdas :: Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  innerLambdas = everywhereOnCpp go
    where
    go :: Cpp -> Cpp
    go f@(CppFunction {}) = maybeRemCaps $ fnToLambda f
    go (CppLambda _ args rtyp body) = maybeRemCaps $ CppLambda [CaptureAll] args rtyp body
    go cpp = cpp
  -------------------------------------------------------------------------------------------------
  convertRecursiveFns :: [Cpp] -> Cpp -> [Cpp]
  -------------------------------------------------------------------------------------------------
  convertRecursiveFns cpps otherCpp
    | not $ null fns = dict : (accessor topdict <$> filteredFns allCpps) ++ cpps'
    where
    cpps' = everywhereOnCpp removeRec <$> cpps
    allCpps = CppBlock $ otherCpp : cpps'
    fns :: [(Text, Cpp)]
    fns = toelem <$> concatMap (everythingOnCpp (++) recursive) cpps
    filteredFns :: Cpp -> [(Text, Cpp)]
    filteredFns cpp = filter (\(f, _) -> everythingOnCpp (||) (== CppVar f) cpp) fns
    recursive :: Cpp -> [Cpp]
    recursive cpp'@(CppFunction _ _ _ qs _)
      | Recursive `elem` qs = [cpp']
    recursive cpp'@(CppVariableIntroduction _ qs _)
      | Recursive `elem` qs = [cpp']
    recursive _ = []
    removeRec :: Cpp -> Cpp
    removeRec (CppFunction _ _ _ qs _)
      | Recursive `elem` qs = CppNoOp
    removeRec (CppVariableIntroduction _ qs _)
      | Recursive `elem` qs = CppNoOp
    removeRec cpp' = cpp'
    replaceVar :: [(Text, Cpp)] -> Cpp -> Cpp
    replaceVar rs = go
      where
      go (CppVar var)
        | Just var' <- lookup var rs = var'
      go cpp' = cpp'
    dict :: Cpp
    dict =
      CppVariableIntroduction
        (topdict, Just $ Any [Const])
        []
        (Just $ CppDataLiteral ((\(f, cpp) -> CppComment [LineComment f] cpp) <$> fns))
    accessed :: Text -> Text -> (Text, Cpp)
    accessed dictname name =
      ( name
      , CppApp
          (CppDataGet
             (CppNumericLiteral . Left . fromIntegral . fromJust $ findIndex ((== name) . fst) fns)
             dict')
          [dict'])
      where
      dict' = CppVar dictname
    accessor :: Text -> (Text, Cpp) -> Cpp
    accessor dictname (name, _) =
      CppVariableIntroduction (name, Just $ Any [Const]) [] (Just . snd $ accessed dictname name)
    toelem :: Cpp -> (Text, Cpp)
    toelem (CppFunction name args rtyp _ body'@(CppBlock body)) =
      ( name
      , withdict . maybeRemCaps $
        CppLambda
          [CaptureAll]
          args
          rtyp
          (CppBlock $ (accessor localdict <$> filteredFns body') ++ body))
    toelem (CppVariableIntroduction (name, _) _ (Just body')) =
      let replacements = accessed localdict . fst <$> filteredFns body'
          replaced = everywhereOnCpp (replaceVar replacements) body'
      in (name, withdict . maybeRemCaps $ CppBlock [CppReturn replaced])
    toelem _ = error "not a function"
    withdict :: Cpp -> Cpp
    withdict cpp' =
      maybeRemCaps $
      CppLambda
        [CaptureAll]
        [(localdict, Just $ Any [Const, Ref])]
        (Just $ Any [])
        (returnBlock cpp')
    topdict :: Text
    topdict = "_$dict$_"
    localdict :: Text
    localdict = "$dict$"
  convertRecursiveFns cpps _ = cpps

  -- |
  -- Generate code in the simplified C++1x intermediate representation for a value or expression.
  --
  -------------------------------------------------------------------------------------------------
  valueToCpp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  valueToCpp (Var (_, _, _, Just IsNewtype) ident) = return $ varToCpp ident
  valueToCpp (Var (_, _, _, Just (IsConstructor _ [])) ident) = return $ CppApp (varToCpp ident) []
  valueToCpp (Var (_, _, Just ty, _) ident@(Qualified (Just _) _))
    | arity ty > 0 = return . curriedName' $ varToCpp ident
  valueToCpp (Var _ ident) = return $ varToCpp ident
  valueToCpp (Literal _ (NumericLiteral n)) = return (CppNumericLiteral n)
  valueToCpp (Literal _ (StringLiteral s)) = return (CppStringLiteral s)
  valueToCpp (Literal _ (CharLiteral c)) = return (CppCharLiteral c)
  valueToCpp (Literal _ (BooleanLiteral b)) = return (CppBooleanLiteral b)
  valueToCpp (Literal _ (ArrayLiteral xs)) = CppArrayLiteral <$> mapM valueToCpp xs
  valueToCpp (Literal _ (ObjectLiteral ps)) =
    CppMapLiteral Record <$>
    mapM (sndM valueToCpp) ((\(k, v) -> (CppSymbol k, v)) <$> sortBy (compare `on` fst) ps)
  valueToCpp (Accessor _ prop val) = CppMapGet <$> pure (CppSymbol prop) <*> valueToCpp val
  -- TODO: use a more efficient way of copying/updating the map?
  valueToCpp (ObjectUpdate (_, _, Just ty, _) obj ps) = do
    obj' <- valueToCpp obj
    updatedFields <- mapM (sndM valueToCpp) ps
    let origKeys = (allKeys ty) \\ (fst <$> updatedFields)
        origFields = (\key -> (key, CppMapGet (CppSymbol key) obj')) <$> origKeys
    return $
      CppMapLiteral Record $
      (\(k, v) -> (CppSymbol k, v)) <$> sortBy (compare `on` fst) (origFields ++ updatedFields)
    where
    allKeys :: T.Type -> [Text]
    allKeys (T.TypeApp (T.TypeConstructor _) r@(T.RCons {})) = fst <$> (fst $ T.rowToList r)
    allKeys (T.ForAll _ t _) = allKeys t
    allKeys _ = error "Not a recognized row type"
  valueToCpp (ObjectUpdate _ _ _) = error $ "Bad Type in object update!"
  valueToCpp (Abs (_, _, ty, _) arg body) = do
    cpp <- valueToCpp body
    let cpp' = innerLambdas cpp
    return $
      CppLambda
        []
        [(identToCpp arg, Just $ Any [Const, Ref])]
        (Just $ Any [])
        (returnBlock $ optIndexers classes cpp')
    where
    arg' = identToCpp arg
    args' = identToCpp <$> (fst $ unAbs body [])
    classes =
      zip
        (CppVar <$> arg' : args')
        (qualifiedToCpp (Ident . runProperName) . T.constraintClass <$>
         maybe [] extractConstraints ty)
  valueToCpp e@App {} = do
    let (f, args) = unApp e []
    args' <- mapM valueToCpp args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just IsTypeClassConstructor) (Qualified mn' (Ident classname)) ->
        let Just (_, constraints, fns) = findClass (Qualified mn' (ProperName classname))
        in return . CppMapLiteral Instance $
           zip (CppSymbol <$> (sort $ superClassDictionaryNames constraints) ++ (fst <$> fns)) args'
      Var _ (Qualified (Just _) _) -> applied f args' curriedName'
      _ -> applied f args' id
    where
    applied f args g = flip (foldl (\fn a -> CppApp (g fn) [a])) args <$> valueToCpp f
  valueToCpp (Case (maybeSpan, _, ty, _) values binders) = do
    vals <- mapM valueToCpp values
    bindersToCpp maybeSpan ty binders vals
  valueToCpp (Let _ ds val) = do
    ds' <- concat <$> mapM (bindToCpp []) ds
    ret <- valueToCpp val
    opts <- ask
    let ds'' = tco opts <$> ds'
    let rs =
          if hasRecursion ds''
            then convertRecursiveFns ds'' ret
            else ds''
    let cpps = innerLambdas <$> rs
    return $ CppApp (CppLambda [] [] Nothing (CppBlock (cpps ++ [CppReturn ret]))) []
    where
    hasRecursion :: [Cpp] -> Bool
    hasRecursion cpps' = any (everythingOnCpp (||) hasRecursiveRef) cpps'
      where
      valnames :: [Text]
      valnames = concatMap (everythingOnCpp (++) valname) cpps'
      valname :: Cpp -> [Text]
      valname (CppFunction name _ _ _ _) = [name]
      valname (CppVariableIntroduction (name, _) _ _) = [name]
      valname _ = []
      hasRecursiveRef :: Cpp -> Bool
      hasRecursiveRef (CppFunction _ _ _ _ body) = everythingOnCpp (||) ref body
      hasRecursiveRef (CppVariableIntroduction _ _ (Just value)) = everythingOnCpp (||) ref value
      hasRecursiveRef _ = False
      ref :: Cpp -> Bool
      ref (CppVar name)
        | name `elem` valnames = True
      ref _ = False
  valueToCpp Constructor {} = return CppNoOp

  -- |
  -- Generate code in the simplified C++1x intermediate representation for a reference to a
  -- variable.
  --
  -------------------------------------------------------------------------------------------------
  varToCpp :: Qualified Ident -> Cpp
  -------------------------------------------------------------------------------------------------
  varToCpp (Qualified Nothing ident) = CppVar (identToCpp ident)
  varToCpp qual = qualifiedToCpp id qual

  -- |
  -- Generate code in the simplified C++1x intermediate representation for pattern match binders
  -- and guards.
  --
  -------------------------------------------------------------------------------------------------
  bindersToCpp :: Maybe SourceSpan -> Maybe T.Type -> [CaseAlternative Ann] -> [Cpp] -> m Cpp
  -------------------------------------------------------------------------------------------------
  bindersToCpp maybeSpan _ binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith mkVarDecl valNames (map Just vals)
    cpps <-
      forM binders $ \(CaseAlternative bs result) -> do
        ret <- guardsToCpp result
        go valNames ret bs
    return $
      CppApp
        (CppLambda
           []
           []
           (Just $ Any [])
           (CppBlock (assignments ++ concat cpps ++ [failedPatternError valNames])))
        []
    where
    mkVarDecl :: Text -> Maybe Cpp -> Cpp
    mkVarDecl name = CppVariableIntroduction (name, Nothing) []
    go :: [Text] -> [Cpp] -> [Binder Ann] -> m [Cpp]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToCpp v done'' b
    go _ _ _ = error "Invalid arguments to bindersToCpp"
    failedPatternError :: [Text] -> Cpp
    failedPatternError _ =
      CppThrow $ CppApp (CppVar "runtime_error") [CppStringLiteral errorMessage]
    errorMessage :: Text
    errorMessage =
      "Failed pattern match" <>
      maybe "" (((" at " <> runModuleName mn <> " ") <>) . displayStartEndPos) maybeSpan <> ": "
    guardsToCpp :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [Cpp]
    guardsToCpp (Left gs) =
      forM gs $ \(cond, val) -> do
        cond' <- valueToCpp cond
        done <- valueToCpp val
        return $ CppIfElse cond' (returnBlock done) Nothing
    guardsToCpp (Right v) = return . CppReturn <$> valueToCpp v

  -- |
  -- Generate code in the simplified C++1x intermediate representation for a pattern match
  -- binder.
  --
  -------------------------------------------------------------------------------------------------
  binderToCpp :: Text -> [Cpp] -> Binder Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  binderToCpp _ done (NullBinder {}) = return done
  binderToCpp varName done (LiteralBinder _ l) = literalToBinderCpp varName done l
  binderToCpp varName done (VarBinder _ ident) =
    return $
    CppVariableIntroduction (identToCpp ident, Just $ Any [Const]) [] (Just $ CppVar varName) : done
  binderToCpp varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToCpp varName done b
  binderToCpp varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ (Qualified mn' (ProperName ctor)) bs) = do
    cpps <- go (zip fields bs) done
    return $
      case ctorType of
        ProductType -> cpps
        SumType ->
          [ CppIfElse
              (CppBinary
                 EqualTo
                 (CppApp (CppVar getCtor) [CppVar varName])
                 ctorCpp)
              (CppBlock cpps)
              Nothing
          ]
    where
    go :: [(Ident, Binder Ann)] -> [Cpp] -> m [Cpp]
    go [] done' = return done'
    go ((field, binder):remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      cpps <- binderToCpp argVar done'' binder
      return $
        CppVariableIntroduction
          (argVar, Nothing)
          []
          (Just $
           CppDataGet
             (fieldToIndex' field)
             (CppVar varName)) :
        cpps
    fieldToIndex :: Ident -> Int
    fieldToIndex = (+ 1) . read . dropWhile isLetter . unpack . runIdent
    fieldToIndex' :: Ident -> Cpp
    fieldToIndex' = CppNumericLiteral . Left . toInteger . fieldToIndex
    ctor' :: Cpp
    ctor' = CppAccessor (CppVar $ safeName ctor) (CppVar ctorNS)
    ctorCpp :: Cpp
    ctorCpp
      | Just mn'' <- mn'
      , mn'' /= mn = CppAccessor ctor' (CppVar $ moduleNameToCpp mn'')
      | otherwise = ctor'
  binderToCpp _ _ b@(ConstructorBinder {}) =
    error $ "Invalid ConstructorBinder in binderToCpp: " <> show b
  binderToCpp varName done (NamedBinder _ ident binder) = do
    cpp <- binderToCpp varName done binder
    return $
      CppVariableIntroduction (identToCpp ident, Just $ Any [Const]) [] (Just $ CppVar varName) :
      cpp
  -------------------------------------------------------------------------------------------------
  literalToBinderCpp :: Text -> [Cpp] -> Literal (Binder Ann) -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  literalToBinderCpp varName done (NumericLiteral num) =
    return
      [ CppIfElse
          (CppBinary EqualTo (CppVar varName) (CppNumericLiteral num))
          (CppBlock done)
          Nothing
      ]
  literalToBinderCpp varName done (CharLiteral c) =
    return
      [CppIfElse (CppBinary EqualTo (CppVar varName) (CppCharLiteral c)) (CppBlock done) Nothing]
  literalToBinderCpp varName done (StringLiteral str) =
    return
      [ CppIfElse
          (CppBinary EqualTo (CppVar varName) (CppStringLiteral str))
          (CppBlock done)
          Nothing
      ]
  literalToBinderCpp varName done (BooleanLiteral True) =
    return [CppIfElse (CppVar varName) (CppBlock done) Nothing]
  literalToBinderCpp varName done (BooleanLiteral False) =
    return [CppIfElse (CppUnary Not (CppVar varName)) (CppBlock done) Nothing]
  literalToBinderCpp varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Cpp] -> [(Text, Binder Ann)] -> m [Cpp]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      cpp <- binderToCpp propVar done'' binder
      return $
        CppVariableIntroduction
          (propVar, Nothing)
          []
          (Just $ CppMapGet (CppSymbol prop) (CppVar varName)) :
        cpp
  literalToBinderCpp varName done (ArrayLiteral bs) = do
    cpp <- go done 0 bs
    let cond =
          case length bs of
            0 -> CppBinary Dot (CppVar varName) (CppApp (CppVar "empty") [])
            n -> CppBinary
                   EqualTo
                   (CppBinary Dot (CppVar varName) (CppApp (CppVar "size") []))
                   (CppNumericLiteral (Left (fromIntegral n)))
    return [CppIfElse cond (CppBlock cpp) Nothing]
    where
    go :: [Cpp] -> Integer -> [Binder Ann] -> m [Cpp]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      cpp <- binderToCpp elVar done'' binder
      return $
        CppVariableIntroduction
          (elVar, Nothing)
          []
          (Just $ CppIndexer (CppNumericLiteral (Left index)) (CppVar varName)) :
        cpp
  -------------------------------------------------------------------------------------------------
  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  -------------------------------------------------------------------------------------------------
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)
  -------------------------------------------------------------------------------------------------
  unAbs :: Expr Ann -> [Ident] -> ([Ident], Expr Ann)
  -------------------------------------------------------------------------------------------------
  unAbs (Abs _ arg val) args = unAbs val (args ++ [arg])
  unAbs other args = (args, other)
  -------------------------------------------------------------------------------------------------
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  -------------------------------------------------------------------------------------------------
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a)
    | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a)
    | mn /= mn' = CppAccessor (CppVar . identToCpp $ f a) (CppVar (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

  -- |
  -- Find a class in scope by name, retrieving its list of constraints, function names and types.
  --
  -------------------------------------------------------------------------------------------------
  findClass :: Qualified (ProperName 'ClassName)
            -> Maybe ([Text], [T.Constraint], [(Text, T.Type)])
  -------------------------------------------------------------------------------------------------
  findClass name
    | Just tcd <- M.lookup name (E.typeClasses env)
    , fns' <- (\(i, t) -> (runIdent i, t)) <$> E.typeClassMembers tcd =
      Just
        ( fst <$> E.typeClassArguments tcd
        , E.typeClassSuperclasses tcd
        , (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing
  -------------------------------------------------------------------------------------------------
  typeClassMembers :: [Ident]
  -------------------------------------------------------------------------------------------------
  typeClassMembers =
    fst <$>
    (concat . M.elems $ M.map E.typeClassMembers (M.filterWithKey thisModule (E.typeClasses env)))
    where
    thisModule (Qualified (Just mn') _) _ = mn' == mn
    thisModule _ _ = False
  -------------------------------------------------------------------------------------------------
  dataCtors :: [Cpp]
  -------------------------------------------------------------------------------------------------
  dataCtors =
    [ CppNamespace
        ctorNS
        [ CppEnum Nothing (Just intType) . ("_" :) . catMaybes . map modCtor . M.keys $
          E.dataConstructors env
        ]
    ]
    where
    modCtor :: Qualified (ProperName a) -> Maybe Text
    modCtor (Qualified (Just mn') (ProperName name))
      | mn' == mn = Just (safeName name)
    modCtor _ = Nothing
  -------------------------------------------------------------------------------------------------
  curriedForeigns :: m [Cpp]
  -------------------------------------------------------------------------------------------------
  curriedForeigns = concat <$> mapM go allForeigns
    where
    go (ident, ty)
      | arity' <- arity ty
      , arity' > 0 = do
        let name = identToCpp ident
        argNames <- replicateM arity' freshName
        return $
          [ CppFunction
              (curriedName name)
              [(head argNames, Just $ Any [Const, Ref])]
              (Just $ Any [])
              (if arity' == 1
                 then [Inline]
                 else [])
              (returnBlock $
               curriedLambda (tail argNames) (CppApp (CppVar name) (CppVar <$> argNames)))
          ]
    go _ = return []
    allForeigns :: [(Ident, T.Type)]
    allForeigns =
      map (\((Qualified _ ident), (ty, _, _)) -> (ident, ty)) .
      filter (\((Qualified mn' _), (_, kind, _)) -> mn' == Just mn && kind == E.External) . M.toList $
      E.names env

---------------------------------------------------------------------------------------------------
returnBlock :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
returnBlock cpp = CppBlock [CppReturn cpp]

---------------------------------------------------------------------------------------------------
curriedLambda :: [Text] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
curriedLambda args ret =
  foldr
    (\arg ret' ->
       CppLambda [CaptureAll] [(arg, Just $ Any [Const, Ref])] (Just $ Any []) (returnBlock ret'))
    ret
    args

---------------------------------------------------------------------------------------------------
curriedApp :: [Text] -> Expr Ann -> Expr Ann
---------------------------------------------------------------------------------------------------
curriedApp args vals =
  foldl (\val arg -> App nullAnn val (Var nullAnn . Qualified Nothing $ Ident arg)) vals args

---------------------------------------------------------------------------------------------------
arity :: T.Type -> Int
---------------------------------------------------------------------------------------------------
arity = go 0
  where
  go arity' (T.ForAll _ t _) = go arity' t
  go arity' (T.TypeApp (T.TypeApp fn _) t)
    | fn == E.tyFunction = go (arity' + 1) t
  go arity' (T.ConstrainedType ts t) = go (arity' + length ts) t
  go arity' _ = arity'

---------------------------------------------------------------------------------------------------
extractConstraints :: T.Type -> [T.Constraint]
---------------------------------------------------------------------------------------------------
extractConstraints = go []
  where
  go cs (T.ForAll _ t _) = go cs t
  go cs (T.TypeApp (T.TypeApp fn _) t)
    | fn == E.tyFunction = go cs t
  go cs (T.ConstrainedType ts t) = go (cs ++ ts) t
  go cs _ = cs

---------------------------------------------------------------------------------------------------
optIndexers :: [(Cpp, Cpp)] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
optIndexers classes = everywhereOnCpp dictIndexerToEnum
  where
  dictIndexerToEnum :: Cpp -> Cpp
  dictIndexerToEnum (CppMapGet (CppSymbol prop) dict)
    | Just cls <- lookup dict classes =
      let index = CppAccessor (CppVar $ safeName prop) cls
      in CppMapGet index dict
  dictIndexerToEnum cpp = cpp

---------------------------------------------------------------------------------------------------
curriedName :: Text -> Text
---------------------------------------------------------------------------------------------------
curriedName name
  | Text.null name = name
curriedName name
  | Text.head name == '*' = name
  | otherwise = '*' `Text.cons` name

---------------------------------------------------------------------------------------------------
curriedName' :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
curriedName' (CppVar name) = CppVar (curriedName name)
curriedName' (CppAccessor a b) = CppAccessor (curriedName' a) b
curriedName' cpp = cpp

---------------------------------------------------------------------------------------------------
primFn :: T.Type -> ([CppType], Maybe CppType)
---------------------------------------------------------------------------------------------------
primFn = go []
  where
  go ps (T.TypeApp (T.TypeApp fn a) b)
    | fn == E.tyFunction
    , Just a' <- numType a = go (ps ++ [a']) b
  go ps t
    | Just t' <- numType t = (ps, Just t')
  go _ _ = ([], Nothing)
  numType t@(T.TypeConstructor {})
    | t == E.tyInt = Just intType
  numType t@(T.TypeConstructor {})
    | t == E.tyNumber = Just doubleType
  numType t@(T.TypeConstructor {})
    | t == E.tyBoolean = Just boolType
  numType t@(T.TypeConstructor {})
    | t == E.tyChar = Just charType
  numType _ = Nothing

---------------------------------------------------------------------------------------------------
convertIfPrimFn :: Maybe T.Type -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
convertIfPrimFn ty@(Just _) (CppNamespace ns fs) = CppNamespace ns (convertIfPrimFn ty <$> fs)
convertIfPrimFn (Just _) cpp@(CppFunction name _ _ _ _)
  | not $ Text.null name
  , Text.head name == '*' = cpp
convertIfPrimFn (Just ty) (CppFunction name params _ qs body)
  | (tys', Just rty') <- primFn ty =
    CppFunction name (zipWith (\(p, _) t -> (p, Just t)) params tys') (Just rty') qs body
convertIfPrimFn _ cpp = cpp

---------------------------------------------------------------------------------------------------
allSymbols :: [Cpp] -> [Text]
---------------------------------------------------------------------------------------------------
allSymbols cpps = nub $ concatMap (everythingOnCpp (++) symbols) cpps
  where
  symbols :: Cpp -> [Text]
  symbols (CppSymbol s) = [s]
  symbols _ = []

---------------------------------------------------------------------------------------------------
filterInlineFuncs :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
filterInlineFuncs = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock []) = CppBlock []
  convert (CppFunction n ps r qs cpp)
    | Inline `elem` qs
    , everythingOnCpp (||) shouldRemove cpp = CppFunction n ps r (delete Inline qs) cpp
    where
    shouldRemove :: Cpp -> Bool
    shouldRemove CppSymbol {} = True
    shouldRemove CppLambda {} = True
    shouldRemove _ = False
  convert cpp = cpp

---------------------------------------------------------------------------------------------------
otherTransforms :: OtherOptions -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
otherTransforms OtherOptions {optionsUseUCNs = True} = everywhereOnCpp unicodeToUCNs
otherTransforms _ = id
