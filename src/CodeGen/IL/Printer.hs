-- | Pretty printer for the AST
module CodeGen.IL.Printer
  ( prettyPrintIL
  , prettyPrintIL1
  , prettyPrintILWithSourceMaps
  , interfaceSource
  , implHeaderSource
  , implFooterSource
  , isLiteral
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import Data.Char (isAscii)
import Data.List (filter, (\\))
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Names (Ident, runIdent)
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString, mkString)
import CodeGen.IL.Common
import CodeGen.IL.Optimizer.TCO (tcoLoop)

import qualified Language.PureScript.Constants as C

-- TODO (Christoph): Get rid of T.unpack / pack

literals :: (Emit gen) => Pattern PrinterState AST gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match' il = (addMapping' (getSourceSpan il) <>) <$> match il

  match :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match (NumericLiteral _ n) = return $ emit $ T.pack $ either show show n
  match (StringLiteral _ s) = return $ emit $ stringLiteral s
  match (BooleanLiteral _ True) = return $ emit "true"
  match (BooleanLiteral _ False) = return $ emit "false"
  match (ArrayLiteral _ xs) = mconcat <$> sequence
    [ return . emit $ arrayType <> "{ "
    , intercalate (emit ", ") <$> forM xs prettyPrintIL'
    , return $ emit " }"
    ]
  -- match (ObjectLiteral _ []) = return $ emit "std::initializer_list<std::pair<const string, boxed>>{}"
  match (ObjectLiteral _ ps) = mconcat <$> sequence
    [ return . emit $ dictType <> "{\n"
    , withIndent $ do
        ils <- forM ps $ \(key, value) -> do
                  value' <- prettyPrintIL' value
                  return $ emit "{ " <> objectPropertyToString key <> value' <> emit " }"
        indentString <- currentIndent
        return $ intercalate (emit ",\n") $ map (indentString <>) ils
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
    where
    objectPropertyToString :: (Emit gen) => PSString -> gen
    objectPropertyToString s = emit $ stringLiteral s <> ", "
  match (Block _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (Var _ ident) | ident == C.undefined = return $ emit C.undefined
  match (Var _ ident) = return $ emit ident
  match (VariableIntroduction _ ident value) = mconcat <$> sequence
    [ return $ emit $ "boxed " <> ident
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintIL') value
    ]
  match (Assignment _ target value) = mconcat <$> sequence
    [ prettyPrintIL' target
    , return $ emit " = "
    , prettyPrintIL' value
    ]
  match (App _ val []) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "()"
    ]
 -- Unbox value
  match (App _ (StringLiteral _ u) [val])
    | Just ty <- decodeString u = mconcat <$> sequence
    [  return . emit $ unbox' ty val
    ]
  match (App _ val args) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "("
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (Function _ (Just name) [] (Block _ [Return _ ret]))
    | isLiteral ret = mconcat <$> sequence
    [ return $ emit "auto "
    , return $ emit name
    , return $ emit "() -> const boxed& "
    , return $ emit "{\n"
    , withIndent $ do
          indentString <- currentIndent
          return $ indentString <> emit staticValueBegin
    , withIndent $ do
        case ret of
          Function _ Nothing args body -> mconcat <$> sequence
            [ return $ emit "[]("
            , return $ emit $ intercalate ", " (renderArg <$> args)
            , return $ emit ") -> boxed "
            , prettyPrintIL' body
            ]
          _ -> do
            prettyPrintIL' ret
    , return $ emit ";\n"
    , withIndent $ do
          indentString <- currentIndent
          return $ indentString <> emit staticValueEnd
    , return $ emit "\n}"
    ]
  match (Function _ (Just name) [] ret) = mconcat <$> sequence
    [ return $ emit "auto "
    , return $ emit name
    , return $ emit "() -> boxed "
    , prettyPrintIL' ret
    ]
  match (Function _ name args ret) = mconcat <$> sequence
    [ return $ emit captures
    , return $ emit "("
    , return $ emit $ intercalate ", " (render <$> args)
    , return $ emit ") -> boxed "
    , prettyPrintIL' ret
    ]
    where
    (captures, render)
      | name == Just tcoLoop = ("[&]", renderArgByVal)
      | otherwise = ("[=]", renderArg)
  match (Indexer _ (Var _ name) (Var _ "")) = mconcat <$> sequence
    [ prettyPrintIL' (Var Nothing name)
    , return $ emit "()"
    ]
  match (Indexer _ prop@(Var _ name) val) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "::"
    , prettyPrintIL' prop
    , return $ emit "()"
    ]
  match (Indexer _ prop@StringLiteral{} val@ObjectLiteral{}) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "["
    , prettyPrintIL' prop
    , return $ emit "]"
    ]
  match (Indexer _ prop val) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "["
    , prettyPrintIL' prop
    , return $ emit "]"
    ]
  match (InstanceOf _ val ty) = mconcat <$> sequence
    [ return . emit $ unbox' dictType val
    , return $ emit ".contains("
    , prettyPrintIL' ty
    , return $ emit ")"
    ]
  match (While _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintIL' cond
    , return $ emit ") "
    , prettyPrintIL' sts
    ]
  match (For _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for (boxed " <> ident <> " = "
    , prettyPrintIL' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintIL' end
    , return $ emit $ "; " <> ident <> "++) "
    , prettyPrintIL' sts
    ]
  match (ForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for (boxed " <> ident <> " in "
    , prettyPrintIL' obj
    , return $ emit ") "
    , prettyPrintIL' sts
    ]
  -- match (IfElse _ (Binary _ EqualTo cond@(Binary{}) (BooleanLiteral Nothing True)) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if ("
  --   , prettyPrintIL' cond
  --   , return $ emit ") "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  -- match (IfElse _ (Binary _ EqualTo cond@(Binary{}) (BooleanLiteral Nothing False)) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if (!("
  --   , prettyPrintIL' cond
  --   , return $ emit ")) "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  -- match (IfElse _ (Binary _ EqualTo cond (BooleanLiteral Nothing True)) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if ("
  --   , return $ emit $ unbox' "bool" cond
  --   , return $ emit ") "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  -- match (IfElse _ (Binary _ EqualTo cond (BooleanLiteral Nothing False)) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if (!("
  --   , return $ emit $ unbox' "bool" cond
  --   , return $ emit ")) "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  -- match (IfElse _ (Binary _ EqualTo x y@(NumericLiteral _ n)) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if ("
  --   , return $ emit $ unbox' t x
  --   , return $ emit $ " == "
  --   , prettyPrintIL' y
  --   , return $ emit ") "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  --   where
  --   t | Left _ <- n  = int
  --     | Right _ <- n = float
  -- match (IfElse _ (Binary _ EqualTo a b@StringLiteral{}) thens elses) = mconcat <$> sequence
  --   [ return $ emit "if ("
  --   , return $ emit $ unbox' string a
  --   , return $ emit $ " == "
  --   , prettyPrintIL' b
  --   , return $ emit ") "
  --   , prettyPrintIL' thens
  --   , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
  --   ]
  match (IfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintIL' cond
    , return $ emit ") "
    , prettyPrintIL' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
    ]
  match (Return _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintIL' value
    ]
  match (ReturnNoResult _) = return . emit $ "return " <> C.undefined
  -- match (Throw _ _) = return mempty
  match (Throw _ value) = mconcat <$> sequence
    [ return $ emit "THROW_("
    , prettyPrintIL' value
    , return $ emit ")"
    ]
  match (Comment _ com il) = mconcat <$> sequence
    [ return $ emit "\n"
    , mconcat <$> forM com comment
    , prettyPrintIL' il
    ]
  match _ = mzero

  comment :: (Emit gen) => Comment -> StateT PrinterState Maybe gen
  comment (LineComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "//" <> emit com <> emit "\n"
    ]
  comment (BlockComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (T.lines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    ]
    where
    asLine :: (Emit gen) => Text -> StateT PrinterState Maybe gen
    asLine s = do
      i <- currentIndent
      return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

    removeComments :: Text -> Text
    removeComments t =
      case T.stripPrefix "*/" t of
        Just rest -> removeComments rest
        Nothing -> case T.uncons t of
          Just (x, xs) -> x `T.cons` removeComments xs
          Nothing -> ""

unary' :: (Emit gen) => UnaryOperator -> (AST -> Text) -> Operator PrinterState AST gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState AST (gen, AST)
  match = mkPattern match'
    where
    match' (Unary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState AST gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState AST gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (Unary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState AST gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState AST (AST, AST)
  match = mkPattern match'
    where
    match' (Binary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

isLiteral :: AST -> Bool
isLiteral Function{} = True
isLiteral NumericLiteral{} = True
isLiteral StringLiteral{} = True
isLiteral BooleanLiteral{} = True
isLiteral ObjectLiteral{} = True
isLiteral ArrayLiteral{} = True
isLiteral _ = False

prettyStatements :: (Emit gen) => [AST] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  ils <- forM sts prettyPrintIL'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ";") . (indentString <>)) ils

-- | Generate a pretty-printed string representing a collection of C++ expressions at the same indentation level
prettyPrintILWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintILWithSourceMaps il =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) il
  in (s, mp)

prettyPrintIL :: [AST] -> Text
prettyPrintIL = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

-- | Generate an indented, pretty-printed string representing a C++ expression
prettyPrintIL' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
prettyPrintIL' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState AST gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState AST gen
  operators =
    OperatorTable [ [ unary New "box" ]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">=" ]
                  , [ binary    EqualTo              "=="
                    , binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                    ]

prettyPrintIL1 :: AST -> Text
prettyPrintIL1 = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyPrintIL'

stringLiteral :: PSString -> Text
stringLiteral pss | Just s <- decodeString pss =
  (if T.all isAscii s
     then ""
     else ("u8")) <> stringLiteral' s
  where
  stringLiteral' :: Text -> Text
  stringLiteral' s = "\"" <> T.concatMap encodeChar s <> "\""
  encodeChar :: Char -> Text
  encodeChar '\0' = "\\0"
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c = T.singleton $ c
stringLiteral _ = "\"\\uFFFD\""

unbox' :: Text -> AST -> Text
unbox' _ v@(NumericLiteral{}) = prettyPrintIL1 v
unbox' _ v@(BooleanLiteral{}) = prettyPrintIL1 v
unbox' _ v@(StringLiteral{})  = prettyPrintIL1 v
unbox' _ v@(Binary{})         = prettyPrintIL1 v
unbox' t v = unbox t <> "(" <> prettyPrintIL1 v <> ")"

interfaceSource :: Text -> [(Text,Bool)] -> [Ident] -> Text
interfaceSource mn values foreigns =
  let foreigns' = identToIL <$> foreigns
      valueNames = fst <$> values
      values' = filter ((`notElem` foreigns') . fst) values ++
                zip (foreigns' \\ valueNames) (repeat True)
  in
  "// Generated by psil compiler\n\n" <>
  "#ifndef " <> mn <> "_H\n" <>
  "#define " <> mn <> "_H\n\n" <>
  "#include \"purescript.h\"\n\n" <>
  "namespace " <> mn <> " {\n\n" <>
  "using namespace purescript;\n\n" <>
  (T.concat $ (\(export, static) ->
                let rty = if static then "const boxed&" else "boxed" in
                "auto " <> export <> "() -> " <> rty <> ";\n") <$> values') <> "\n" <>
  "} // end namespace " <> mn <> "\n\n" <>
  "#endif // " <> mn <> "_H\n\n"

implHeaderSource :: Text -> [Text] -> Text -> Text
implHeaderSource mn imports interfaceImport =
  T.concat imports <> "\n"<>
  interfaceImport <> "\n\n" <>
  "namespace " <> mn <> " {\n\n"

implFooterSource :: Text -> [Ident] -> Text
implFooterSource mn foreigns =
  "\n\n\n" <>
  (if null foreigns
    then ""
    else ("// Foreign values\n\n" <>
          "DEFINE_FOREIGN_DICTIONARY_AND_ACCESSOR()\n\n" <>
          (T.concat $ (\foreign' -> "auto " <>
                                    identToIL foreign' <>
                                    "() -> const boxed& { " <>
                                    staticValueBegin <>
                                    foreignDict <> "().at(" <>
                                        (stringLiteral . mkString $ runIdent foreign') <> "); " <>
                                    staticValueEnd <>
                                    " };\n") <$> foreigns) <>
          "\n")) <>
  "} // end namespace " <> mn <>
  "\n\n" <>
  if mn == "Main" then mainSource else "\n"
  where
  mainSource :: Text
  mainSource = "\
    \int main(int argc, const char * argv[]) {\n\
    \    Main::main()();\n\
    \    return 0;\n\
    \}\n\n\
    \"

varDecl :: Text
varDecl = "const boxed"

varRefDecl :: Text
varRefDecl = varDecl <> "&"

renderArg' :: Text -> Text -> Text
renderArg' decl arg
  | arg == unusedName = decl
  | otherwise = decl <> " " <> arg

renderArg :: Text -> Text
renderArg = renderArg' varRefDecl

renderArgByVal :: Text -> Text
renderArgByVal = renderArg' varDecl

foreignDict :: Text
foreignDict = "foreign"

staticValueBegin :: Text
staticValueBegin = "static const boxed _ = "

staticValueEnd :: Text
staticValueEnd = "return _;"
