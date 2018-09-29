-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module CodeGen.Cpp
  ( module AST
  , module Common
  , moduleToCpp
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow ((&&&))
import Control.Monad (forM, liftM, replicateM, void)
import Control.Monad.Supply.Class

import Data.List ((\\), delete, intersect)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreImp.AST (AST)
import qualified Language.PureScript.CoreImp.AST as AST
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import System.FilePath.Posix ((</>))

import CodeGen.Cpp.Common as Common
import CodeGen.Cpp.Optimizer
import CodeGen.Cpp.Printer

data DeclType = ModuleDecl | LetDecl | RecLetDecl deriving (Eq)

-- | Generate code in the simplified intermediate representation for all declarations in a
-- module.
moduleToCpp
  :: forall m
   -- . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
   . (Monad m, MonadSupply m)
  => Module Ann
  -> Maybe AST
  -> m (Text, [Text], [AST], Text, Text)
moduleToCpp (Module _ coms mn _ imps exports foreigns decls) _ =
  do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    cppImports <- traverse (importToCpp mnLookup) . (\\ (mn : C.primModules)) . (\\ [mn]) $ ordNub $ map snd imps
    interfaceImport <- importToCpp (renameImports [] [(emptyAnn, mn)]) mn
    let decls' = renameModules mnLookup decls
    cppDecls <- mapM (bindToCpp ModuleDecl) decls'
    optimized <- traverse (traverse optimize) cppDecls
    let optimized' = concat optimized
        exports' = mapMaybe (export $ identToCpp <$> exports) optimized'
        foreigns' = identToCpp <$> foreigns
        interface = interfaceSource modName exports' foreigns
        implHeader = implHeaderSource modName cppImports interfaceImport
        implFooter = implFooterSource modName foreigns
    return $ (interface, foreigns', optimized', implHeader, implFooter)
  where
  modName = moduleNameToCpp mn

  export :: [Text] -> AST -> Maybe (Text, Bool)
  export names (AST.Function _ (Just name) [] (AST.Block _ [AST.Return _ ret]))
    | name `elem` names && isLiteral ret = Just (name, True)
  export names (AST.Function _ (Just name) [] _)
    | name `elem` names = Just (name, False)
  export _ _ = Nothing

  -- | Extracts all declaration names from a binding group.
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- | Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
  renameImports = go M.empty
    where
    go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
    go acc used ((ann, mn') : mns') =
      let mni = Ident $ runModuleName mn'
      in if mn' /= mn && mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' (ann, mn') acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName pns) used =
      let newName = ModuleName $ init pns ++ [ProperName $ runProperName (last pns) <> "_" <> T.pack (show i)]
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- | Generates C++ code for a module import, binding the required module
  -- to the alternative
  importToCpp :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m Text
  importToCpp mnLookup mn' = do
    let ((_, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
        mname = moduleNameToCpp mnSafe
    pure $ "#include \"" <> mname <> "/" <> mname <> ".h\"\n"

  -- | Replaces the `ModuleName`s in the AST so that the generated code refers to
  -- the collision-avoiding renamed module imports.
  renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
  renameModules mnLookup binds =
    let (f, _, _) = everywhereOnValues id goExpr goBinder
    in map f binds
    where
    goExpr :: Expr a -> Expr a
    goExpr (Var ann q) = Var ann (renameQual q)
    goExpr e = e
    goBinder :: Binder a -> Binder a
    goBinder (ConstructorBinder ann q1 q2 bs) = ConstructorBinder ann (renameQual q1) (renameQual q2) bs
    goBinder b = b
    renameQual :: Qualified a -> Qualified a
    renameQual (Qualified (Just mn') a) =
      let (_,mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in Qualified (Just mnSafe) a
    renameQual q = q

  -- |
  -- Generate code in the simplified intermediate representation for a declaration
  --
  bindToCpp :: DeclType -> Bind Ann -> m [AST]
  bindToCpp dt (NonRec ann ident val) = return <$> nonRecToCpp dt ann ident val
  bindToCpp dt (Rec vals) = forM vals (uncurry . uncurry $ nonRecToCpp dt')
    where
    dt' = if dt == LetDecl then RecLetDecl else dt

  -- | Generate code in the simplified intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  nonRecToCpp :: DeclType -> Ann -> Ident -> Expr Ann -> m AST
  nonRecToCpp dt a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    -- withoutComment <- asks optionsNoComments
    let withoutComment = False
    if withoutComment
       then nonRecToCpp dt a i (modifyAnn removeComments e)
       else AST.Comment Nothing com <$> nonRecToCpp dt a i (modifyAnn removeComments e)
  nonRecToCpp ModuleDecl (ss, _, _, _) ident val = do
    cpp <- valueToCpp val
    pure $ AST.Function Nothing (Just $ identToCpp ident) [] (AST.Block Nothing [AST.Return Nothing cpp])
  nonRecToCpp RecLetDecl (ss, _, _, _) ident val = do
    cpp <- valueToCpp val
    pure $
      let retainedName = identToCpp ident
          retained = AST.Var Nothing $ retainedName
          -- unretained = AST.Var Nothing $ retainedName <> unretainedSuffix
          -- alias = AST.Var Nothing . -- Note: prevents optimizer from eliminating this
          --             prettyPrintCpp1 $
          --               AST.VariableIntroduction Nothing retainedName (Just unretained)
      in
      case cpp of
        AST.Function _ name args (AST.Block Nothing cpps) ->
          AST.Assignment Nothing
              (retained)
              (AST.Function Nothing
                   name
                   args 
                   (AST.Block Nothing cpps))
        _ -> AST.Assignment Nothing retained cpp
  nonRecToCpp _ (ss, _, _, _) ident val = do
    cpp <- valueToCpp val
    pure $ AST.VariableIntroduction Nothing (identToCpp ident) (Just cpp)

  -- | Generate code in the simplified intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToCpp

  -- | Generate code in the simplified intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in C++ (symbol based, reserved name) an
  -- indexer is returned.
  accessor :: Ident -> AST -> AST
  accessor (Ident prop) = accessorString $ mkString prop
  accessor (GenIdent _ _) = internalError "GenIdent in accessor"
  accessor UnusedIdent = internalError "UnusedIdent in accessor"

  accessorString :: PSString -> AST -> AST
  accessorString prop = AST.Indexer Nothing (AST.StringLiteral Nothing prop)

  -- | Generate code in the simplified intermediate representation for a value or expression.

  valueToCpp :: Expr Ann -> m AST
  valueToCpp (Literal (pos, _, _, _) l) = literalToValueCpp pos l
  valueToCpp (Accessor _ prop val) =
    accessorString prop <$> valueToCpp val
  valueToCpp (ObjectUpdate _ o ps) = do
    obj <- valueToCpp o
    updates <- mapM (sndM valueToCpp) ps
    obj' <- freshName
    let objVar = AST.Var Nothing obj'
        copy = AST.VariableIntroduction Nothing obj' (Just $ AST.App Nothing (AST.Var Nothing (unbox dictType)) [obj])
        assign (k, v) = AST.Assignment Nothing (accessorString k objVar) v
        sts = copy : (assign <$> updates) ++ [AST.Return Nothing objVar]
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing sts)) []
  valueToCpp (Abs _ arg val) = do
    ret <- valueToCpp val
    let cppArg = case arg of
                  UnusedIdent -> []
                  _           -> [identToCpp arg]
    return $ AST.Function Nothing Nothing cppArg (AST.Block Nothing [AST.Return Nothing ret])
  valueToCpp e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToCpp args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      _ -> flip (foldl (\fn a -> AST.App Nothing fn [a])) args' <$> valueToCpp f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  -- valueToCpp (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
  --   return $ if mn' == mn
  --            then AST.Var Nothing (moduleNameToCpp mn' <> "::" <> identToCpp ident)
  --            else varToCpp qi
  -- valueToCpp (Var (_, _, _, Just IsForeign) (Qualified (Just mn') ident)) =
  --   return $ AST.Var Nothing (moduleNameToCpp mn' <> "::" <> identToCpp ident)
  -- valueToCpp (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
  --   return $ if mn' == mn
  --            then foreignIdent ident
  --            else varToCpp qi
  -- valueToCpp (Var (_, _, _, Just IsForeign) ident) =
  --   internalError $ "Encountered an unqualified reference to a foreign ident " ++ T.unpack (showQualified showIdent ident)
  valueToCpp (Var _ ident) = return $ varToCpp ident
  valueToCpp (Case (ss, _, _, _) values binders) = do
    vals <- mapM valueToCpp values
    bindersToCpp ss binders vals
  valueToCpp (Let _ ds val) = do
    let recurs = concatMap getNames $ filter isRec ds
        recurDs = (\v -> AST.VariableIntroduction Nothing (identToCpp v) (Just (AST.Var Nothing "box<fn_t>()"))) <$> recurs
        -- recurDsWeak = (\v -> AST.VariableIntroduction Nothing ("__unsafe_unretained " <> identToCpp v <> unretainedSuffix) Nothing) <$> recurs
    ds' <- concat <$> mapM (bindToCpp LetDecl) ds
    ret <- valueToCpp val
    return $
      AST.App Nothing
          (AST.Function Nothing
               Nothing
               []
               (AST.Block Nothing (recurDs ++ ds' ++ [AST.Return Nothing ret])))
          []
    where
    isRec :: Bind Ann -> Bool
    isRec (Rec _) = True
    isRec _ = False      
  valueToCpp (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) = error "IsNewtype"
  valueToCpp (Constructor _ _ (ProperName ctor) fields) =
    let body = AST.ObjectLiteral Nothing $ (mkString ctor, AST.BooleanLiteral Nothing True) :
                                           ((\field -> (mkString (identToCpp field), var field)) `map` fields)
        fn = foldr (\f inner -> AST.Function Nothing Nothing [identToCpp f] (AST.Block Nothing [AST.Return Nothing inner])) body fields
    in return fn


  iife :: Text -> [AST] -> AST
  iife v exprs = AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing $ exprs ++ [AST.Return Nothing $ AST.Var Nothing v])) []

  literalToValueCpp :: SourceSpan -> Literal (Expr Ann) -> m AST
  literalToValueCpp ss (NumericLiteral (Left i)) = return $ AST.NumericLiteral (Just ss) (Left i)
  literalToValueCpp ss (NumericLiteral (Right n)) = return $ AST.NumericLiteral (Just ss) (Right n)
  literalToValueCpp ss (StringLiteral s) = return $ AST.StringLiteral (Just ss) s
  literalToValueCpp ss (CharLiteral c) = return $ AST.StringLiteral (Just ss) (fromString [c])
  literalToValueCpp ss (BooleanLiteral b) = return $ AST.BooleanLiteral (Just ss) b
  literalToValueCpp ss (ArrayLiteral xs) = AST.ArrayLiteral (Just ss) <$> mapM valueToCpp xs
  literalToValueCpp ss (ObjectLiteral ps) = AST.ObjectLiteral (Just ss) <$> mapM (sndM valueToCpp) ps

  -- | Generate code in the simplified intermediate representation for a reference to a
  -- variable.
  varToCpp :: Qualified Ident -> AST
  varToCpp (Qualified Nothing ident) = var ident
  varToCpp qual = qualifiedToCpp id qual

  -- | Generate code in the simplified intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> AST
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = AST.Var Nothing . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a) = AST.Indexer Nothing (AST.Var Nothing . identToCpp $ f a) (AST.Var Nothing (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = AST.Var Nothing $ identToCpp (f a)

  -- foreignIdent :: Ident -> AST
  -- foreignIdent ident = accessorString (mkString $ runIdent ident) (AST.Var Nothing "$foreign")

  -- | Generate code in the simplified intermediate representation for pattern match binders
  -- and guards.
  bindersToCpp :: SourceSpan -> [CaseAlternative Ann] -> [AST] -> m AST
  bindersToCpp ss binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (AST.VariableIntroduction Nothing) valNames (map Just vals)
    cpps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToCpp result
      go valNames ret bs
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (assignments ++ concat cpps ++ [AST.Throw Nothing (AST.StringLiteral Nothing $ mkString failedPatternMessage)])))
                   []
    where
      go :: [Text] -> [AST] -> [Binder Ann] -> m [AST]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToCpp v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToCpp"

      failedPatternMessage :: Text
      failedPatternMessage = "Failed pattern match at " <> runModuleName mn <> " " <> displayStartEndPos ss <> ": "

      guardsToCpp :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [AST]
      guardsToCpp (Left gs) = traverse genGuard gs where
        genGuard (cond, val) = do
          cond' <- valueToCpp cond
          val'   <- valueToCpp val
          return
            (AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo cond' (AST.BooleanLiteral Nothing True))
              (AST.Block Nothing [AST.Return Nothing val']) Nothing)

      guardsToCpp (Right v) = return . AST.Return Nothing <$> valueToCpp v

  -- | Generate code in the simplified intermediate representation for a pattern match
  -- binder.
  binderToCpp :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToCpp _ done NullBinder{} = return done
  binderToCpp varName done (LiteralBinder _ l) =
    literalToBinderCpp varName done l
  binderToCpp varName done (VarBinder _ ident) =
    return (AST.VariableIntroduction Nothing (identToCpp ident) (Just (AST.Var Nothing varName)) : done)
  binderToCpp varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToCpp varName done b
  binderToCpp varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ (Qualified _ (ProperName ctor)) bs) = do
    cpp <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> cpp
      SumType ->
        [AST.IfElse Nothing (AST.InstanceOf Nothing (AST.Var Nothing varName) (AST.StringLiteral Nothing (mkString ctor)))
                  (AST.Block Nothing cpp)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [AST] -> m [AST]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      cpp <- binderToCpp argVar done'' binder
      return (AST.VariableIntroduction Nothing argVar (Just $ accessorString (mkString $ identToCpp field) $ AST.Var Nothing varName) : cpp)

  binderToCpp _ _ ConstructorBinder{} =
    internalError "binderToCpp: Invalid ConstructorBinder in binderToCpp"
  binderToCpp varName done (NamedBinder _ ident binder) = do
    cpp <- binderToCpp varName done binder
    return (AST.VariableIntroduction Nothing (identToCpp ident) (Just (AST.Var Nothing varName)) : cpp)

  literalToBinderCpp :: Text -> [AST] -> Literal (Binder Ann) -> m [AST]
  literalToBinderCpp varName done (NumericLiteral num) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.NumericLiteral Nothing num)) (AST.Block Nothing done) Nothing]
  literalToBinderCpp varName done (CharLiteral c) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing (fromString [c]))) (AST.Block Nothing done) Nothing]
  literalToBinderCpp varName done (StringLiteral str) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing str)) (AST.Block Nothing done) Nothing]
  literalToBinderCpp varName done (BooleanLiteral True) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.BooleanLiteral Nothing True)) (AST.Block Nothing done) Nothing]
  literalToBinderCpp varName done (BooleanLiteral False) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.BooleanLiteral Nothing False)) (AST.Block Nothing done) Nothing]
    -- return [AST.IfElse Nothing (AST.Unary Nothing AST.Not (AST.Var Nothing varName)) (AST.Block Nothing done) Nothing]
  literalToBinderCpp varName done (ObjectLiteral bs) = go done bs
    where
    go :: [AST] -> [(PSString, Binder Ann)] -> m [AST]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      cpp <- binderToCpp propVar done'' binder
      return (AST.VariableIntroduction Nothing propVar (Just (accessorString prop (AST.Var Nothing varName))) : cpp)
  literalToBinderCpp varName done (ArrayLiteral bs) = do
    cpp <- go done 0 bs
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo
                                   (arrayLength $ AST.Var Nothing varName)
                                   (AST.NumericLiteral Nothing (Left (fromIntegral $ length bs))))
               (AST.Block Nothing cpp) Nothing]
    where
    go :: [AST] -> Integer -> [Binder Ann] -> m [AST]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      cpp <- binderToCpp elVar done'' binder
      return (AST.VariableIntroduction Nothing elVar (Just (AST.Indexer Nothing (AST.NumericLiteral Nothing (Left index)) (AST.Var Nothing varName))) : cpp)

emptyAnn :: Ann
emptyAnn = (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0), [], Nothing, Nothing)

arrayLength :: AST -> AST
arrayLength a = AST.App Nothing (AST.Var Nothing arrayLengthFn) [a]
