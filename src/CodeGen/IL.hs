-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module CodeGen.IL
  ( module AST
  , module Common
  , moduleToIL
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow ((&&&))
import Control.Monad (forM, liftM, replicateM, void)
import Control.Monad.Supply.Class

import Data.List ((\\), delete, intersect, nub)
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

import CodeGen.IL.Common as Common
import CodeGen.IL.Optimizer
import CodeGen.IL.Printer

data DeclType = ModuleDecl | LetDecl | RecLetDecl deriving (Eq)

-- | Generate code in the simplified intermediate representation for all declarations in a
-- module.
moduleToIL
  :: forall m
   -- . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
   . (Monad m, MonadSupply m)
  => Module Ann
  -> Maybe AST
  -> m (Text, [Text], [AST], Text, Text)
moduleToIL (Module _ coms mn _ imps _ foreigns decls) _ =
  do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    let decls' = renameModules mnLookup decls
    ilDecls <- mapM (bindToIL ModuleDecl) decls'
    optimized <- traverse (traverse (optimize modName')) ilDecls
    let optimized' = concat optimized
        values = annotValue <$> optimized'
        foreigns' = identToIL <$> foreigns
        interface = interfaceSource modName values foreigns
        imports = nub . concat $ importToIL <$> optimized'
        moduleHeader = importToIL' modName
        implHeader = implHeaderSource modName imports moduleHeader
        implFooter = implFooterSource (runModuleName mn) foreigns
    return $ (interface, foreigns', optimized', implHeader, implFooter)
  where
  modName = moduleNameToIL mn
  modName' = AST.Var Nothing modName

  annotValue :: AST -> (Text, Bool)
  annotValue (AST.Function _ (Just name) [] (AST.Block _ [AST.Return _ ret]))
    | isLiteral ret = (name, True)
  annotValue (AST.Function _ (Just name) [] _) = (name, False)
  annotValue _ = error "Unexpected top-level value form"

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

  -- | Generates IL code for a module import
  --
  importToIL :: AST -> [Text]
  importToIL = AST.everything (++) modRef
    where
    modRef (AST.Indexer _ (AST.Var _ _) (AST.Var _ mname))
      | not $ T.null mname = [importToIL' mname]
    modRef _ = []
  importToIL' :: Text -> Text
  importToIL' h = "#include \"" <> h <> "/" <> h <> ".h\"\n"

  -- | Replaces the `ModuleName`s in the AST so that the generated code refers to
  -- the collision-avoiding renamed module imports.
  renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
  renameModules mnLookup binds =
    let (f, _, _) = everywhereOnValues id ilExpr goBinder
    in map f binds
    where
    ilExpr :: Expr a -> Expr a
    ilExpr (Var ann q) = Var ann (renameQual q)
    ilExpr e = e
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
  bindToIL :: DeclType -> Bind Ann -> m [AST]
  bindToIL dt (NonRec ann ident val) = return <$> nonRecToIL dt ann ident val
  bindToIL dt (Rec vals) = forM vals (uncurry . uncurry $ nonRecToIL dt')
    where
    dt' = if dt == LetDecl then RecLetDecl else dt

  -- | Generate code in the simplified intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  nonRecToIL :: DeclType -> Ann -> Ident -> Expr Ann -> m AST
  nonRecToIL dt a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    -- withoutComment <- asks optionsNoComments
    let withoutComment = False
    if withoutComment
       then nonRecToIL dt a i (modifyAnn removeComments e)
       else AST.Comment Nothing com <$> nonRecToIL dt a i (modifyAnn removeComments e)
  nonRecToIL ModuleDecl (ss, _, _, _) ident val = do
    il <- valueToIL val
    pure $ AST.Function Nothing (Just $ identToIL ident) [] (AST.Block Nothing [AST.Return Nothing il])
  nonRecToIL RecLetDecl (ss, _, _, _) ident val = do
    il <- valueToIL val
    pure $
      let retainedName = identToIL ident
          retained = AST.Var Nothing $ retainedName
          unretainedName = retainedName <> unretainedSuffix
          unretained = AST.Var Nothing $ unretainedName
          boxed = AST.Var Nothing . -- Note: prevents optimizer from eliminating this
                      prettyPrintIL1 $
                        AST.VariableIntroduction
                            Nothing
                            retainedName
                            (Just unretained)
          alias = AST.Var Nothing $ "auto& " <> retainedName <> " = " <> unretainedName
      in
      case il of
        AST.Function _ name args (AST.Block Nothing ils) ->
          AST.Assignment Nothing
              retained
              (AST.Function Nothing
                   name
                   args 
                   (AST.Block Nothing (boxed : ils)))
        _ ->
          AST.Assignment Nothing
              retained
              (AST.App Nothing
                  (AST.Function Nothing
                      Nothing
                      []
                      (AST.Block Nothing ([ alias
                                          , AST.Return Nothing il
                                          ])))
               [])
  nonRecToIL _ (ss, _, _, _) ident val = do
    il <- valueToIL val
    pure $ AST.VariableIntroduction Nothing (identToIL ident) (Just il)

  -- | Generate code in the simplified intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToIL

  accessorString :: PSString -> AST -> AST
  accessorString prop = AST.Indexer Nothing (AST.StringLiteral Nothing prop)

  -- | Generate code in the simplified intermediate representation for a value or expression.

  valueToIL :: Expr Ann -> m AST
  valueToIL (Literal (pos, _, _, _) l) = literalToValueIL pos l
  valueToIL (Accessor _ prop val) =
    accessorString prop <$> valueToIL val
  valueToIL (ObjectUpdate _ o ps) = do
    obj <- valueToIL o
    updates <- mapM (sndM valueToIL) ps
    obj' <- freshName'
    let objVar = AST.Var Nothing obj'
        copy = AST.VariableIntroduction Nothing obj' (Just $ AST.App Nothing (AST.Var Nothing (unbox dictType)) [obj])
        assign (k, v) = AST.Assignment Nothing (accessorString k objVar) v
        sts = copy : (assign <$> updates) ++ [AST.Return Nothing objVar]
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing sts)) []
  valueToIL (Abs _ arg val) = do
    ret <- valueToIL val
    let ilArg = case arg of
                  UnusedIdent -> []
                  _           -> [identToIL arg]
    return $ AST.Function Nothing Nothing ilArg (AST.Block Nothing [AST.Return Nothing ret])
  valueToIL e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToIL args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      _ -> flip (foldl (\fn a -> AST.App Nothing fn [a])) args' <$> valueToIL f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToIL (Var _ ident) = return $ varToIL ident
  valueToIL (Case (ss, _, _, _) values binders) = do
    vals <- mapM valueToIL values
    bindersToIL ss binders vals
  valueToIL (Let _ ds val) = do
    let recurs = concatMap getNames $ filter isRec ds
        recurDs = (\v -> AST.Var Nothing $ "boxed::recur " <> identToIL v) <$> recurs
        recurDsWeak = (\v -> AST.Var
                             Nothing
                             ("boxed::recur::weak " <> identToIL v <> unretainedSuffix <> "(" <> identToIL v <> ")")
                       ) <$> recurs
        mutualRecurWarning = if any isMutRec $ filter isRec ds
                               then [AST.Var Nothing "#pragma message(\"Mutually recursive lets will cause retain cycles (memory leaks)\") //"]
                               else []
    ds' <- concat <$> mapM (bindToIL LetDecl) ds
    ret <- valueToIL val
    return $
      AST.App Nothing
          (AST.Function Nothing
               Nothing
               []
               (AST.Block Nothing (recurDs ++ recurDsWeak ++ mutualRecurWarning ++ ds' ++ [AST.Return Nothing ret])))
          []
    where
    isRec :: Bind Ann -> Bool
    isRec (Rec _) = True
    isRec _ = False
    isMutRec :: Bind Ann -> Bool
    isMutRec b | (length . nub $ getNames b) > 1 = True
               | otherwise = False
  valueToIL (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) = error "IsNewtype"
  valueToIL (Constructor _ _ (ProperName ctor) fields) =
    let body = AST.ObjectLiteral Nothing $ (mkString ctor, AST.BooleanLiteral Nothing True) :
                                           ((\field -> (mkString (identToIL field), var field)) `map` fields)
        fn = foldr (\f inner -> AST.Function Nothing Nothing [identToIL f] (AST.Block Nothing [AST.Return Nothing inner])) body fields
    in return fn


  iife :: Text -> [AST] -> AST
  iife v exprs = AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing $ exprs ++ [AST.Return Nothing $ AST.Var Nothing v])) []

  literalToValueIL :: SourceSpan -> Literal (Expr Ann) -> m AST
  literalToValueIL ss (NumericLiteral (Left i)) = return $ AST.NumericLiteral (Just ss) (Left i)
  literalToValueIL ss (NumericLiteral (Right n)) = return $ AST.NumericLiteral (Just ss) (Right n)
  literalToValueIL ss (StringLiteral s) = return $ AST.StringLiteral (Just ss) s
  literalToValueIL ss (CharLiteral c) = return $ AST.StringLiteral (Just ss) (fromString [c])
  literalToValueIL ss (BooleanLiteral b) = return $ AST.BooleanLiteral (Just ss) b
  literalToValueIL ss (ArrayLiteral xs) = AST.ArrayLiteral (Just ss) <$> mapM valueToIL xs
  literalToValueIL ss (ObjectLiteral ps) = AST.ObjectLiteral (Just ss) <$> mapM (sndM valueToIL) ps

  -- | Generate code in the simplified intermediate representation for a reference to a
  -- variable.
  varToIL :: Qualified Ident -> AST
  varToIL (Qualified Nothing ident) = var ident
  varToIL qual = qualifiedToIL id qual

  -- | Generate code in the simplified intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToIL :: (a -> Ident) -> Qualified a -> AST
  qualifiedToIL f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = AST.Var Nothing . identToIL $ f a
  qualifiedToIL f (Qualified (Just mn') a) | mn /= mn' = AST.Indexer Nothing (AST.Var Nothing . identToIL $ f a) (AST.Var Nothing (moduleNameToIL mn'))
  qualifiedToIL f (Qualified _ a) = AST.Indexer Nothing (AST.Var Nothing . identToIL $ f a) (AST.Var Nothing "")

  -- foreignIdent :: Ident -> AST
  -- foreignIdent ident = accessorString (mkString $ runIdent ident) (AST.Var Nothing "$foreign")

  -- | Generate code in the simplified intermediate representation for pattern match binders
  -- and guards.
  bindersToIL :: SourceSpan -> [CaseAlternative Ann] -> [AST] -> m AST
  bindersToIL ss binders vals = do
    valNames <- replicateM (length vals) freshName'
    let assignments = zipWith (AST.VariableIntroduction Nothing) valNames (map Just vals)
    ils <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToIL result
      go valNames ret bs
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (assignments ++ concat ils ++ [AST.Throw Nothing (AST.StringLiteral Nothing $ mkString failedPatternMessage)])))
                   []
    where
      go :: [Text] -> [AST] -> [Binder Ann] -> m [AST]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToIL v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToIL"

      failedPatternMessage :: Text
      failedPatternMessage = "Failed pattern match at " <> runModuleName mn <> " " <> displayStartEndPos ss

      guardsToIL :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [AST]
      guardsToIL (Left gs) = traverse genGuard gs where
        genGuard (cond, val) = do
          cond' <- valueToIL cond
          val'   <- valueToIL val
          return
            (AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo cond' (AST.BooleanLiteral Nothing True))
              (AST.Block Nothing [AST.Return Nothing val']) Nothing)

      guardsToIL (Right v) = return . AST.Return Nothing <$> valueToIL v

  -- | Generate code in the simplified intermediate representation for a pattern match
  -- binder.
  binderToIL :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToIL _ done NullBinder{} = return done
  binderToIL varName done (LiteralBinder _ l) =
    literalToBinderIL varName done l
  binderToIL varName done (VarBinder _ ident) =
    return (AST.VariableIntroduction Nothing (identToIL ident) (Just (AST.Var Nothing varName)) : done)
  binderToIL varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToIL varName done b
  binderToIL varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ (Qualified _ (ProperName ctor)) bs) = do
    il <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> il
      SumType ->
        [AST.IfElse Nothing (AST.InstanceOf Nothing (AST.Var Nothing varName) (AST.StringLiteral Nothing (mkString ctor)))
                  (AST.Block Nothing il)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [AST] -> m [AST]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName'
      done'' <- go remain done'
      il <- binderToIL argVar done'' binder
      return (AST.VariableIntroduction Nothing argVar (Just $ accessorString (mkString $ identToIL field) $ AST.Var Nothing varName) : il)

  binderToIL _ _ ConstructorBinder{} =
    internalError "binderToIL: Invalid ConstructorBinder in binderToIL"
  binderToIL varName done (NamedBinder _ ident binder) = do
    il <- binderToIL varName done binder
    return (AST.VariableIntroduction Nothing (identToIL ident) (Just (AST.Var Nothing varName)) : il)

  literalToBinderIL :: Text -> [AST] -> Literal (Binder Ann) -> m [AST]
  literalToBinderIL varName done (NumericLiteral num) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.NumericLiteral Nothing num)) (AST.Block Nothing done) Nothing]
  literalToBinderIL varName done (CharLiteral c) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing (fromString [c]))) (AST.Block Nothing done) Nothing]
  literalToBinderIL varName done (StringLiteral str) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing str)) (AST.Block Nothing done) Nothing]
  literalToBinderIL varName done (BooleanLiteral True) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.BooleanLiteral Nothing True)) (AST.Block Nothing done) Nothing]
  literalToBinderIL varName done (BooleanLiteral False) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.BooleanLiteral Nothing False)) (AST.Block Nothing done) Nothing]
    -- return [AST.IfElse Nothing (AST.Unary Nothing AST.Not (AST.Var Nothing varName)) (AST.Block Nothing done) Nothing]
  literalToBinderIL varName done (ObjectLiteral bs) = go done bs
    where
    go :: [AST] -> [(PSString, Binder Ann)] -> m [AST]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName'
      done'' <- go done' bs'
      il <- binderToIL propVar done'' binder
      return (AST.VariableIntroduction Nothing propVar (Just (accessorString prop (AST.Var Nothing varName))) : il)
  literalToBinderIL varName done (ArrayLiteral bs) = do
    il <- go done 0 bs
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo
                                   (arrayLength $ AST.Var Nothing varName)
                                   (AST.NumericLiteral Nothing (Left (fromIntegral $ length bs))))
               (AST.Block Nothing il) Nothing]
    where
    go :: [AST] -> Integer -> [Binder Ann] -> m [AST]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName'
      done'' <- go done' (index + 1) bs'
      il <- binderToIL elVar done'' binder
      return (AST.VariableIntroduction Nothing elVar (Just (AST.Indexer Nothing (AST.NumericLiteral Nothing (Left index)) (AST.Var Nothing varName))) : il)

emptyAnn :: Ann
emptyAnn = (SourceSpan "" (SourcePos 0 0) (SourcePos 0 0), [], Nothing, Nothing)

arrayLength :: AST -> AST
arrayLength a = AST.App Nothing (AST.Var Nothing arrayLengthFn) [a]

freshName' :: MonadSupply m => m Text
freshName' = do
    name <- freshName
    return $ T.replace "$" "_Local_" name
