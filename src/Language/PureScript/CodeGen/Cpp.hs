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
  , P.prettyPrintCpp
  ) where

import Prelude.Compat

import Data.Char (isLetter)
import Data.List
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as M

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

-- |
-- Generate code in the simplified C++1x intermediate representation for all declarations in a
-- module.
--
---------------------------------------------------------------------------------------------------
moduleToCpp :: forall m. (Applicative m, Monad m, MonadReader Options m, MonadSupply m)
           => E.Environment -> Module Ann -> m [Cpp]
---------------------------------------------------------------------------------------------------
moduleToCpp env (Module _ mn imps _ foreigns decls) = do
    cppImports <-
        traverse (pure . runModuleName) .
        delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $
        (snd <$> imps)
    let cppImports' = "PureScript" : cppImports
    cppDecls <- concat <$> mapM (bindToCpp [CppTopLevel]) decls
    let namesmap = M.toList .
                   M.mapKeysMonotonic (qualifiedToCpp id) . M.map (\(t, _, _) -> arity t) $
                   E.names env
        datamap =  M.toList .
                   M.mapKeysMonotonic (qualifiedToCpp (Ident . runProperName)) .
                   M.map (\(_, _, _, fields) -> length fields) $
                   E.dataConstructors env
    cppDecls' <- traverse (optimize $ namesmap ++ datamap) cppDecls
    foreignWrappers <- curriedForeigns
    let optimized = cppDecls' ++ foreignWrappers
    let moduleHeader =
            fileBegin mn "HH" ++
            P.linebreak ++
            ((\i -> CppInclude i i) <$> cppImports') ++
            (if not (null foreigns)
                 then [CppInclude [] (runModuleName mn ++ "_ffi")]
                 else []) ++
            P.linebreak ++
            [ CppNamespace (runModuleName mn) $
                  (CppUseNamespace <$> cppImports') ++
                  P.linebreak ++
                  dataCtors ++
                  toHeader optimized ++
                  toHeaderFns optimized
            ] ++
            P.linebreak ++
            fileEnd mn "HH"
    let bodyCpps = toBodyDecl optimized ++ toBody optimized
        moduleBody =
            CppInclude (runModuleName mn) (runModuleName mn) :
            P.linebreak ++
            (if null bodyCpps
                 then []
                 else [ CppNamespace (runModuleName mn) $
                        (CppUseNamespace <$> cppImports') ++
                        P.linebreak ++ bodyCpps ]) ++
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
  bindToCpp :: [CppValueQual] -> Bind Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  bindToCpp qs (NonRec ann ident val) = return <$> declToCpp qs (ann, ident) val
  bindToCpp qs (Rec vals) = forM vals (uncurry $ declToCpp (CppRecursive:qs))

  -- |
  -- Desugar a declaration into a variable introduction or named function
  -- declaration.
  -------------------------------------------------------------------------------------------------
  declToCpp :: [CppValueQual] -> (Ann, Ident) -> Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  declToCpp _ (_, ident) e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let className = identToCpp ident
        (supers, members) = span (C.__superclass_ `isPrefixOf`) (identToCpp <$> (fst $ unAbs e []))
    in return $ CppStruct className [CppEnum Nothing Nothing (sort supers ++ members)]

  declToCpp vqs (_, ident) (Abs (_, com, ty, _) arg body) = do
    fn <- if CppTopLevel `elem` vqs
            then do
              argNames <- replicateM (arity' - length args' - 1) freshName
              let args'' = zip argNames (repeat aty)
              block <- asReturnBlock <$> valueToCpp
                         (if null argNames
                            then snd abs'
                            else curriedApp
                                     (tail argNames)
                                     (App
                                        nullAnn
                                        (snd abs')
                                        (Var nullAnn . Qualified Nothing . Ident $ head argNames)))
              return $ CppNamespace ""
                           [ CppFunction
                                 name
                                 (arg' : args' ++ args'')
                                 rty
                                 (vqs ++ if isAccessor (snd abs') then [CppInline] else [])
                                 (convertNestedLambdas $ convertDictIndexers block)
                           , CppFunction
                                 (curriedName name)
                                 [arg']
                                 rty
                                 (if arity' == 1 then [CppInline] else [])
                                 (asReturnBlock $
                                      curriedLambda
                                          (fst <$> args' ++ args'')
                                          (CppApp (CppVar name)
                                                  (CppVar . fst <$> (arg' : args' ++ args''))))
                           ]
            else do
              block <- valueToCpp body
              return $ CppFunction
                           name
                           [arg']
                           rty
                           (vqs ++ if isIndexer block then [CppInline] else [])
                           (convertNestedLambdas . asReturnBlock $ convertDictIndexers block)
    return (CppComment com $ convertIfPrimFn ty' fn)
    where
    ty' | Just t <- ty = Just t
        | Just (t, _, _) <- M.lookup (Qualified (Just mn) ident) (E.names env) = Just t
        | otherwise = Nothing
    arity' = maybe 0 arity ty'
    name = identToCpp ident
    aty = Just $ CppAny [CppConst, CppRef]
    rty = Just $ CppAny []
    arg' = (identToCpp arg, aty)
    args' = (\i -> (identToCpp i, aty)) <$> fst abs'
    abs' = unAbs body []
    isAccessor :: Expr Ann -> Bool
    isAccessor Accessor{} = True
    isAccessor _ = False
    isIndexer :: Cpp -> Bool
    isIndexer (CppIndexer (CppStringLiteral _) (CppVar _)) = True
    isIndexer _ = False
    classes = qualifiedToCpp (Ident . runProperName) . T.constraintClass <$>
                  maybe [] extractConstraints ty'
    dictClasses = zip (CppVar . fst <$> arg' : args') classes
    convertDictIndexers = everywhereOnCpp (dictIndexerToEnum dictClasses)

  declToCpp _ (_, ident) (Constructor _ _ (ProperName _) []) =
    return $
      CppFunction
          (identToCpp ident)
          []
          (Just $ CppAny [])
          [CppInline]
          (asReturnBlock $
               CppDataLiteral [CppAccessor (CppVar $ identToCpp ident) (CppVar "data")])

  declToCpp _ (_, ident) (Constructor _ _ (ProperName _) fields) =
    return . CppNamespace "" $
    [ CppFunction
          name
          (farg <$> fields')
          (Just $ CppAny [])
          [CppInline]
          (asReturnBlock $
               CppDataLiteral $
                   CppAccessor (CppVar name) (CppVar "data") : (CppVar <$> fields'))
    ] ++
    if length fields > 0
      then
        [ CppFunction
              (curriedName name)
              (farg <$> f)
              (Just $ CppAny [])
              (if length fields == 1 then [CppInline] else [])
              (CppBlock (fieldLambdas fs))
        ]
      else
        []
    where
    name = identToCpp ident
    fields' = identToCpp <$> fields
    (f, fs) | (f' : fs') <- fields' = ([f'], fs')
            | otherwise = ([], [])
    farg = \x -> (x, Just $ CppAny [CppConst, CppRef])
    fieldLambdas flds
      | (f' : fs') <- flds = [ CppReturn . maybeRemCaps $
                               CppLambda
                                   [CppCaptureAll]
                                   (farg <$> [f'])
                                   (Just $ CppAny [])
                                   (CppBlock $ fieldLambdas fs') ]
      | otherwise = [CppReturn $ CppApp (CppVar name) (CppVar <$> fields')]

  declToCpp qs (_, ident) e
    | Just ty <- exprType e,
      CppTopLevel `elem` qs,
      arity' <- arity ty,
      arity' > 0 = do
        argNames <- replicateM arity' freshName
        block <- valueToCpp $
                   curriedApp
                       (tail argNames)
                       (App nullAnn e (Var nullAnn . Qualified Nothing . Ident $ head argNames))
        let fn' = CppFunction
                     (curriedName name)
                     [(head argNames, Just $ CppAny [CppConst, CppRef])]
                     rty
                     []
                     (asReturnBlock $
                          curriedLambda
                              (tail argNames)
                              (CppApp (CppVar name)
                                      (CppVar <$> argNames)))
        return $ CppNamespace "" $
                     [ CppFunction
                           name
                           (zip argNames $ repeat aty)
                           rty
                           []
                           (asReturnBlock block)
                     ] ++ if arity' > 0
                            then [fn']
                            else []
    where
    name = identToCpp ident
    aty = Just $ CppAny [CppConst, CppRef]
    rty = Just $ CppAny []

  declToCpp _ (_, ident) val = do
    val' <- valueToCpp val
    return $
      CppVariableIntroduction
          (identToCpp ident, Just $ CppAny [CppConst])
          []
          (Just val')

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
        (name, Just $ CppAny [CppConst])
        (filter (==CppStatic) qs)
        (Just $ CppLambda cs args (Just $ CppAny []) body)
    where
    cs | (not . null) (qs `intersect` [CppInline, CppStatic]) = []
       | otherwise = [CppCaptureAll]
  fnToLambda (CppComment com cpp) = CppComment com (fnToLambda cpp)
  fnToLambda _ = error "Not a function!"

  -------------------------------------------------------------------------------------------------
  convertNestedLambdas :: Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  convertNestedLambdas = everywhereOnCpp go
    where
    go :: Cpp -> Cpp
    go f@(CppFunction {}) = maybeRemCaps $ fnToLambda f
    go (CppLambda _ args rtyp body) = maybeRemCaps $ CppLambda [CppCaptureAll] args rtyp body
    go cpp = cpp

  -------------------------------------------------------------------------------------------------
  convertRecursiveFns :: [Cpp] -> Cpp -> [Cpp]
  -------------------------------------------------------------------------------------------------
  convertRecursiveFns cpps otherCpp
    | not $ null fns = dict : (accessor topdict <$> filteredFns allCpps) ++ cpps'
    where
    cpps' = everywhereOnCpp removeRec <$> cpps
    allCpps = CppBlock $ otherCpp : cpps'

    fns :: [(String, Cpp)]
    fns = toelem <$> concatMap (everythingOnCpp (++) recursive) cpps

    filteredFns :: Cpp -> [(String, Cpp)]
    filteredFns cpp = filter (\(f,_) -> everythingOnCpp (||) (== CppVar f) cpp) fns

    recursive :: Cpp -> [Cpp]
    recursive cpp'@(CppFunction _ _ _ qs _) | CppRecursive `elem` qs = [cpp']
    recursive _ = []

    removeRec :: Cpp -> Cpp
    removeRec(CppFunction _ _ _ qs _) | CppRecursive `elem` qs = CppNoOp
    removeRec cpp' = cpp'

    dict :: Cpp
    dict = CppVariableIntroduction
               (topdict, Just $ CppAny [CppConst])
               []
               (Just $ CppDataLiteral ((\(f,cpp) -> CppComment [LineComment f] cpp) <$> fns))

    accessor :: String -> (String, Cpp) -> Cpp
    accessor dictname (name, _) =
      CppVariableIntroduction
          (name, Just $ CppAny [CppConst])
          []
          (Just accessed)
      where
      dict' = CppVar dictname
      dict'' = CppCast dataType dict'
      accessed = CppApp
                   (CppIndexer
                      (CppNumericLiteral .
                       Left .
                       fromIntegral .
                       fromJust $ findIndex ((== name) . fst) fns)
                      dict'')
                   [dict']

    toelem :: Cpp -> (String, Cpp)
    toelem (CppFunction name args rtyp _ body'@(CppBlock body)) =
      (name, withdict . maybeRemCaps $ CppLambda
                                           [CppCaptureAll]
                                           args
                                           rtyp
                                           (CppBlock $ (accessor localdict <$> filteredFns body') ++ body))
    toelem _ = error "not a function"

    withdict :: Cpp -> Cpp
    withdict cpp' = maybeRemCaps $ CppLambda
                                       [CppCaptureAll]
                                       [(localdict, Just $ CppAny [CppConst, CppRef])]
                                       (Just $ CppAny [])
                                       (asReturnBlock cpp')
    topdict :: String
    topdict = "_$dict$_"
    localdict :: String
    localdict = "$dict$"

  convertRecursiveFns cpps _ = cpps

  -- |
  -- Generate code in the simplified C++1x intermediate representation for a value or expression.
  --
  -------------------------------------------------------------------------------------------------
  valueToCpp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  valueToCpp (Var (_, _, _, Just IsNewtype) ident) =
    return $ varToCpp ident

  valueToCpp (Var (_, _, _, Just (IsConstructor _ [])) ident) =
    return $ CppApp (varToCpp ident) []

  valueToCpp (Var (_, _, Just ty, _) ident@(Qualified (Just _) _))
    | arity ty > 0 =
      return . curriedName' $ varToCpp ident

  valueToCpp (Var _ ident) =
    return $ varToCpp ident

  valueToCpp (Literal _ (NumericLiteral n)) =
    return (CppNumericLiteral n)

  valueToCpp (Literal _ (StringLiteral s)) =
    return (CppStringLiteral s)

  valueToCpp (Literal _ (CharLiteral c)) =
    return (CppCharLiteral c)

  valueToCpp (Literal _ (BooleanLiteral b)) =
    return (CppBooleanLiteral b)

  valueToCpp (Literal _ (ArrayLiteral xs)) =
    CppArrayLiteral <$> mapM valueToCpp xs

  valueToCpp (Literal _ (ObjectLiteral ps)) =
    CppObjectLiteral CppRecord <$> sortBy (compare `on` fst) <$> mapM (sndM valueToCpp) ps

  valueToCpp (Accessor _ prop val) =
    CppIndexer <$> pure (CppStringLiteral prop) <*> valueToCpp val

  -- TODO: use a more efficient way of copying/updating the map?
  valueToCpp (ObjectUpdate (_, _, Just ty, _) obj ps) = do
    obj' <- valueToCpp obj
    updatedFields <- mapM (sndM valueToCpp) ps
    let origKeys = (allKeys ty) \\ (fst <$> updatedFields)
        origFields = (\key -> (key, CppIndexer (CppStringLiteral key) obj')) <$> origKeys
    return $ CppObjectLiteral CppRecord . sortBy (compare `on` fst) $ origFields ++ updatedFields
    where
    allKeys :: T.Type -> [String]
    allKeys (T.TypeApp (T.TypeConstructor _) r@(T.RCons {})) = fst <$> (fst $ T.rowToList r)
    allKeys (T.ForAll _ t _) = allKeys t
    allKeys _ = error $ show "Not a recognized row type"

  valueToCpp (ObjectUpdate _ _ _) = error $ "Bad Type in object update!"

  valueToCpp (Abs (_, _, ty, _) arg body) = do
    cpp <- valueToCpp body
    let cpp' = convertNestedLambdas cpp
    return $
      CppLambda
          []
          [(identToCpp arg, Just $ CppAny [CppConst, CppRef])]
          (Just $ CppAny [])
          (asReturnBlock $ convertDictIndexers cpp')
    where
      arg' = identToCpp arg
      args' = identToCpp <$> (fst $ unAbs body [])
      classes = qualifiedToCpp (Ident . runProperName) . T.constraintClass <$>
                    maybe [] extractConstraints ty
      dictClasses = zip (CppVar <$> arg' : args') classes
      convertDictIndexers = everywhereOnCpp (dictIndexerToEnum dictClasses)
  valueToCpp e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToCpp args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just IsTypeClassConstructor) (Qualified mn' (Ident classname)) ->
        let Just (_, constraints, fns) = findClass (Qualified mn' (ProperName classname)) in
        return . CppObjectLiteral CppInstance $
                     zip
                       ((sort $ superClassDictionaryNames constraints) ++ (fst <$> fns))
                       args'
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
    let rs = if hasRecursion ds''
               then convertRecursiveFns ds'' ret
               else ds''
    let cpps = convertNestedLambdas <$> rs
    return $ CppApp
                 (CppLambda [] [] Nothing (CppBlock (cpps ++ [CppReturn ret])))
                 []
    where
    hasRecursion :: [Cpp] -> Bool
    hasRecursion cpps' = any (everythingOnCpp (||) hasRecursiveRef) cpps'
      where
      fnames :: [String]
      fnames  = concatMap (everythingOnCpp (++) fname) cpps'

      fname :: Cpp -> [String]
      fname (CppFunction name _ _ _ _) = [name]
      fname _ = []

      hasRecursiveRef :: Cpp -> Bool
      hasRecursiveRef (CppFunction _ _ _ _ body) = everythingOnCpp (||) ref body
      hasRecursiveRef _ = False

      ref :: Cpp -> Bool
      ref (CppVar name) | name `elem` fnames = True
      ref _ = False

  valueToCpp Constructor{} = return CppNoOp

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
    cpps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToCpp result
      go valNames ret bs
    return $
      CppApp
        (CppLambda
             []
             []
             (Just $ CppAny [])
             (CppBlock (assignments ++ concat cpps ++ [failedPatternError valNames])))
        []
    where
      mkVarDecl :: String -> Maybe Cpp -> Cpp
      mkVarDecl name = CppVariableIntroduction (name, Nothing) []
      go :: [String] -> [Cpp] -> [Binder Ann] -> m [Cpp]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToCpp v done'' b
      go _ _ _ = error "Invalid arguments to bindersToCpp"

      failedPatternError :: [String] -> Cpp
      failedPatternError _ =
        CppThrow $ CppApp (CppVar "runtime_error") [CppStringLiteral errorMessage]

      errorMessage :: String
      errorMessage = "Failed pattern match" ++
                     maybe "" (((" at " ++ runModuleName mn ++ " ") ++) . displayStartEndPos) maybeSpan ++
                     ": "

      guardsToCpp :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [Cpp]
      guardsToCpp (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToCpp cond
        done  <- valueToCpp val
        return $ CppIfElse cond' (asReturnBlock done) Nothing
      guardsToCpp (Right v) = return . CppReturn <$> valueToCpp v

  -- |
  -- Generate code in the simplified C++1x intermediate representation for a pattern match
  -- binder.
  --
  -------------------------------------------------------------------------------------------------
  binderToCpp :: String -> [Cpp] -> Binder Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  binderToCpp _ done (NullBinder{}) = return done

  binderToCpp varName done (LiteralBinder _ l) =
    literalToBinderCpp varName done l

  binderToCpp varName done (VarBinder _ ident) =
    return $
      CppVariableIntroduction
          (identToCpp ident, Just $ CppAny [CppConst])
          []
          (Just $ CppVar varName)
      : done

  binderToCpp varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToCpp varName done b

  binderToCpp varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields))
                                              _
                                              (Qualified mn' (ProperName ctor))
                                              bs) = do
    cpps <- go (zip fields bs) done
    return $
      case ctorType of
        ProductType -> cpps
        SumType ->
            [ CppIfElse
                  (CppBinary
                       EqualTo
                       (CppIndexer
                            (CppVar ctorKey)
                            (CppCast dataType $ CppVar varName))
                       ctorCpp)
                  (CppBlock cpps)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [Cpp] -> m [Cpp]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      cpps <- binderToCpp argVar done'' binder
      return $
        CppVariableIntroduction
            (argVar, Nothing)
            []
            (Just $ CppIndexer
                        (fieldToIndex field)
                        (CppCast dataType $ CppVar varName))
        : cpps
    fieldToIndex :: Ident -> Cpp
    fieldToIndex = CppNumericLiteral . Left . (+1) . read . dropWhile isLetter . runIdent
    ctor' :: Cpp
    ctor' = CppAccessor (CppVar . identToCpp $ Ident ctor) (CppVar "data")
    ctorCpp :: Cpp
    ctorCpp | Just mn'' <- mn', mn'' /= mn = CppAccessor ctor' (CppVar $ moduleNameToCpp mn'')
            | otherwise = ctor'

  binderToCpp _ _ b@(ConstructorBinder{}) =
    error $ "Invalid ConstructorBinder in binderToCpp: " ++ show b

  binderToCpp varName done (NamedBinder _ ident binder) = do
    cpp <- binderToCpp varName done binder
    return $
      CppVariableIntroduction
          (identToCpp ident, Just $ CppAny [CppConst])
          []
          (Just $ CppVar varName)
      : cpp

  -------------------------------------------------------------------------------------------------
  literalToBinderCpp :: String -> [Cpp] -> Literal (Binder Ann) -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  literalToBinderCpp varName done (NumericLiteral num) =
    return [ CppIfElse
                 (CppBinary EqualTo (CppVar varName) (CppNumericLiteral num))
                 (CppBlock done)
                 Nothing ]

  literalToBinderCpp varName done (CharLiteral c) =
    return [ CppIfElse
                 (CppBinary EqualTo (CppVar varName) (CppCharLiteral c))
                 (CppBlock done)
                 Nothing ]

  literalToBinderCpp varName done (StringLiteral str) =
    return [ CppIfElse
                 (CppBinary EqualTo (CppVar varName) (CppStringLiteral str))
                 (CppBlock done)
                 Nothing ]

  literalToBinderCpp varName done (BooleanLiteral True) =
    return [ CppIfElse
                 (CppVar varName)
                 (CppBlock done)
                 Nothing ]

  literalToBinderCpp varName done (BooleanLiteral False) =
    return [ CppIfElse
                 (CppUnary
                      Not
                      (CppVar varName))
                 (CppBlock done)
                 Nothing ]

  literalToBinderCpp varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Cpp] -> [(String, Binder Ann)] -> m [Cpp]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      cpp <- binderToCpp propVar done'' binder
      return $
        CppVariableIntroduction
            (propVar, Nothing)
            []
            (Just $ CppIndexer
                        (CppStringLiteral prop)
                        (CppVar varName))
        : cpp

  literalToBinderCpp varName done (ArrayLiteral bs) = do
    cpp <- go done 0 bs
    let cond =
          case length bs of
              0 -> CppBinary
                       Dot
                       (CppCast arrayType $ CppVar varName)
                       (CppApp (CppVar "empty") [])
              n -> let var = CppCast arrayType $ CppVar varName
                   in CppBinary
                          EqualTo
                          (CppBinary Dot var (CppApp (CppVar "size") []))
                          (CppNumericLiteral (Left (fromIntegral n)))
    return [ CppIfElse cond (CppBlock cpp) Nothing ]
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
            (Just $ CppIndexer
                        (CppNumericLiteral (Left index))
                        (CppVar varName))
        : cpp

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
  findClass :: Qualified (ProperName 'ClassName) -> Maybe ([String], [T.Constraint], [(String, T.Type)])
  -------------------------------------------------------------------------------------------------
  findClass name
    | Just (params, fns, constraints) <- M.lookup name (E.typeClasses env),
      fns' <- (\(i,t) -> (runIdent i, t)) <$> fns
      = Just (fst <$> params, constraints, (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing

  -------------------------------------------------------------------------------------------------
  dataCtors :: [Cpp]
  -------------------------------------------------------------------------------------------------
  dataCtors = [CppNamespace "data"
                   [CppEnum Nothing (Just intType) . ("_" :) .
                        catMaybes . map modCtor . M.keys $ E.dataConstructors env]]
    where
    modCtor :: Qualified (ProperName a) -> Maybe String
    modCtor (Qualified (Just mn') (ProperName name)) | mn' == mn = Just name
    modCtor _ = Nothing

  -------------------------------------------------------------------------------------------------
  curriedForeigns :: m [Cpp]
  -------------------------------------------------------------------------------------------------
  curriedForeigns = concat <$> mapM go allForeigns
    where
    go (ident, ty)
      | arity' <- arity ty,
        arity' > 0 = do
          let name = identToCpp ident
          argNames <- replicateM arity' freshName
          return $
            [ CppFunction
                  (curriedName name)
                  [(head argNames, Just $ CppAny [CppConst, CppRef])]
                  (Just $ CppAny [])
                  (if arity' == 1 then [CppInline] else [])
                  (asReturnBlock $
                      curriedLambda
                          (tail argNames)
                          (CppApp (CppVar name)
                                  (CppVar <$> argNames)))
            ]
    go _ = return []

    allForeigns :: [(Ident, T.Type)]
    allForeigns = map (\((Qualified _ ident), (ty, _, _)) -> (ident, ty)) .
                   filter (\((Qualified mn' _), (_, kind, _)) -> mn' == Just mn && kind == E.External) .
                   M.toList $ E.names env

---------------------------------------------------------------------------------------------------
asReturnBlock :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
asReturnBlock cpp = CppBlock [CppReturn cpp]

---------------------------------------------------------------------------------------------------
curriedLambda :: [String] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
curriedLambda args ret = foldr (\arg ret' -> CppLambda
                                                 [CppCaptureAll]
                                                 [(arg, Just $ CppAny [CppConst, CppRef])]
                                                 (Just $ CppAny [])
                                                 (asReturnBlock ret')
                                ) ret args

---------------------------------------------------------------------------------------------------
curriedApp :: [String] -> Expr Ann -> Expr Ann
---------------------------------------------------------------------------------------------------
curriedApp args vals = foldl (\val arg -> App
                                            nullAnn
                                            val
                                            (Var nullAnn . Qualified Nothing $ Ident arg))
                             vals args

---------------------------------------------------------------------------------------------------
arity :: T.Type -> Int
---------------------------------------------------------------------------------------------------
arity = go 0
  where
  go arity' (T.ForAll _ t _) = go arity' t
  go arity' (T.TypeApp (T.TypeApp fn _) t) | fn == E.tyFunction = go (arity' + 1) t
  go arity' (T.ConstrainedType ts t) = go (arity' + length ts) t
  go arity' _ = arity'

---------------------------------------------------------------------------------------------------
extractConstraints :: T.Type -> [T.Constraint]
---------------------------------------------------------------------------------------------------
extractConstraints = go []
  where
  go cs (T.ForAll _ t _) = go cs t
  go cs (T.TypeApp (T.TypeApp fn _) t) | fn == E.tyFunction = go cs t
  go cs (T.ConstrainedType ts t) = go (cs ++ ts) t
  go cs _ = cs

---------------------------------------------------------------------------------------------------
dictIndexerToEnum :: [(Cpp,Cpp)] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
dictIndexerToEnum repls (CppIndexer (CppStringLiteral prop) dict)
  | Just cls <- lookup dict repls =
    CppBinary Dot
      (CppIndexer
          (CppAccessor (CppVar . identToCpp $ Ident prop) cls)
          (CppCast mapType dict))
      (CppVar "second")
dictIndexerToEnum _ cpp = cpp

---------------------------------------------------------------------------------------------------
curriedName :: String -> String
---------------------------------------------------------------------------------------------------
curriedName name@('$' : _) = name
curriedName name = '$' : name

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
  go ps (T.TypeApp (T.TypeApp fn a) b) | fn == E.tyFunction,
                                         Just a' <- numType a = go (ps ++ [a']) b
  go ps t | Just t' <- numType t = (ps , Just t')
  go _ _ = ([], Nothing)
  numType t@(T.TypeConstructor {}) | t == E.tyInt = Just intType
  numType t@(T.TypeConstructor {}) | t == E.tyNumber = Just doubleType
  numType t@(T.TypeConstructor {}) | t == E.tyBoolean = Just boolType
  numType t@(T.TypeConstructor {}) | t == E.tyChar = Just charType
  numType _ = Nothing

---------------------------------------------------------------------------------------------------
convertIfPrimFn :: Maybe T.Type -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
convertIfPrimFn ty@(Just _) (CppNamespace ns fs) = CppNamespace ns (convertIfPrimFn ty <$> fs)
convertIfPrimFn (Just _) cpp@(CppFunction ('$':_) _ _ _ _) = cpp
convertIfPrimFn (Just ty) (CppFunction name params _ qs body)
  | (tys', Just rty') <- primFn ty =
    CppFunction name (zipWith (\(p, _) t -> (p, Just t)) params tys') (Just rty') qs body
convertIfPrimFn _ cpp = cpp
