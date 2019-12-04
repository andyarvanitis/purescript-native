-- | Pretty printer for the AST
module CodeGen.IL.Printer
  ( prettyPrintIL
  , prettyPrintIL1
  , prettyPrintILWithSourceMaps
  , interfaceSource
  , implHeaderSource
  , implFooterSource
  , isLiteral
  , modLabel
  , modPrefix
  , ffiLoader
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
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
    [ return . emit $ arrayType <> "{"
    , intercalate (emit ", ") <$> forM xs prettyPrintIL'
    , return $ emit "}"
    ]
  -- match (ObjectLiteral _ []) = return $ emit "std::initializer_list<std::pair<const string, boxed>>{}"
  match (ObjectLiteral _ ps) = mconcat <$> sequence
    [ return . emit $ dictType <> "{"
    , withIndent $ do
        ils <- forM ps $ \(key, value) -> do
                  value' <- prettyPrintIL' value
                  return $ objectPropertyToString key <> value'
        return $ intercalate (emit " ") $ map (<> emit ",") ils
    , return $ emit "}"
    ]
    where
    objectPropertyToString :: (Emit gen) => PSString -> gen
    objectPropertyToString s = emit $ stringLiteral s <> ": "
  match (Block _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
  match (Var _ ident) | ident == C.undefined = return $ emit undefinedName
  match (Var _ ident) = return $ emit ident
  match (VariableIntroduction _ ident value) = mconcat <$> sequence
    [ return . emit $ varDecl <> " " <> ident <> " " <> anyType
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintIL') value
    ]
  match (Assignment _ target value) = mconcat <$> sequence
    [ prettyPrintIL' target
    , return $ emit " = "
    , prettyPrintIL' value
    ]
  match (App _ val []) = mconcat <$> sequence
    [ return $ emit "Run("
    , prettyPrintIL' val
    , return $ emit ")"
    ]
  match (App _ (StringLiteral _ u) [arg])
    | Just ty <- decodeString u = mconcat <$> sequence
    [ prettyPrintIL' arg
    , return $ emit ".("
    , return $ emit ty
    , return $ emit ")"
    ]
  match (App _ (Var _ fn) [arg]) | fn == arrayLengthFn = mconcat <$> sequence
    [ return $ emit fn
    , return $ emit "("
    , prettyPrintIL' arg
    , return $ emit ")"
    ]
  match (App _ (Var _ fn) args) | fn == indexFn = mconcat <$> sequence
    [ return $ emit fn
    , return $ emit "("
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (App _ (Var _ fn) args) | fn == tcoLoop = mconcat <$> sequence
    [ return $ emit fn
    , return $ emit "("
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (App _ (App Nothing (StringLiteral Nothing fnx) [fn@Function{}]) args)
    | Just ftype <- decodeString fnx
    , "Fn" `T.isPrefixOf` ftype = mconcat <$> sequence
    [ prettyPrintIL' fn
    , return $ emit "("
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (App _ (App Nothing (StringLiteral Nothing fnx) [fn]) args)
    | Just ftype <- decodeString fnx
    , "Fn" `T.isPrefixOf` ftype = mconcat <$> sequence
    [ prettyPrintIL' fn
    , return $ emit ".("
    , return $ emit ftype
    , return $ emit ")("
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match app@(App{})
    | (val, args) <- unApp app []
    , length args > 1
    = mconcat <$> sequence
    [ return $ emit "Apply("
    , prettyPrintIL' val
    , return $ emit ", "
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (App _ val args) = mconcat <$> sequence
    [ return $ emit "Apply("
    , prettyPrintIL' val
    , return $ emit ", "
    , intercalate (emit ", ") <$> forM args prettyPrintIL'
    , return $ emit ")"
    ]
  match (Function _ (Just name) [] (Block _ [Return _ ret]))
    | isComplexLiteral ret = mconcat <$> sequence
    [ return $ emit "\n"
    , return . emit $ varDecl <> " " <> initName name <> " Once\n"
    , prettyPrintIL' $ VariableIntroduction Nothing (valueName name) Nothing
    , return $ emit "\n\n"
    , return $ emit "func "
    , return . emit $ withPrefix name
    , return . emit $ "() " <> anyType <> " "
    , return $ emit "{\n"
    , withIndent $ do
        indentString <- currentIndent
        return $ indentString <> (emit $ initName name <> ".Do(func() {\n") <>
                 indentString <> indentString <> (emit $ valueName name <> " = ")
    , withIndent $ do
        case ret of
          Function _ Nothing args body -> mconcat <$> sequence
            [ return $ emit "func("
            , return . emit $ intercalate ", " (renderArg anyType <$> args)
            , return . emit $ ") " <> anyType <> " "
            , prettyPrintIL' body
            ]
          _ -> do
            prettyPrintIL' ret
    , return $ emit "\n"
    , withIndent $ do
        indentString <- currentIndent
        return $ indentString <> (emit $ "})\n" <> "return " <> valueName name <>"\n")
    , return $ emit "}\n\n"
    ]
  match (Function _ (Just name) [] ret) = mconcat <$> sequence
    [ return $ emit "func "
    , return . emit $ withPrefix name
    , return . emit $ "() " <> anyType <> " "
    , prettyPrintIL' ret
    ]
  match (Function _ _ args ret) = mconcat <$> sequence
    [ return $ emit "func("
    , return . emit $ intercalate ", " (renderArg anyType <$> args)
    , return . emit $ ") " <> anyType <> " "
    , prettyPrintIL' ret
    ]
  match (Indexer _ (Var _ name) (Var _ "")) = mconcat <$> sequence
    [ prettyPrintIL' (Var Nothing $ withPrefix name)
    , return $ emit "()"
    ]
  match (Indexer _ (Var _ name) val) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "."
    , prettyPrintIL' (Var Nothing $ withPrefix name)
    , return $ emit "()"
    ]
  match (Indexer _ prop@StringLiteral{} val@ObjectLiteral{}) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "["
    , prettyPrintIL' prop
    , return $ emit "]"
    ]
  match (Indexer _ prop@StringLiteral{} val) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit ".("
    , return $ emit dictType
    , return $ emit ")["
    , prettyPrintIL' prop
    , return $ emit "]"
    ]
  match (Indexer _ index val@ArrayLiteral{}) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit "["
    , prettyPrintIL' index
    , return $ emit "]"
    ]
  match (Indexer _ index val) = mconcat <$> sequence
    [ prettyPrintIL' val
    , return $ emit ".("
    , return $ emit arrayType
    , return $ emit ")["
    , prettyPrintIL' index
    , return $ emit "]"
    ]
  match (InstanceOf _ val ty) = mconcat <$> sequence
    [ return $ emit "Contains("
    , prettyPrintIL' val
    , return $ emit ", "
    , prettyPrintIL' ty
    , return $ emit ")"
    ]
  match (While _ cond sts) = mconcat <$> sequence
    [ return $ emit "for "
    , prettyPrintIL' cond
    , return $ emit " "
    , prettyPrintIL' sts
    ]
  match (For _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for var " <> ident <> " = "
    , prettyPrintIL' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintIL' end
    , return $ emit $ "; " <> ident <> "++ "
    , prettyPrintIL' sts
    ]
  match (ForIn _ ident obj sts) = mconcat <$> sequence
    [ return $ emit $ "for var " <> ident <> " in "
    , prettyPrintIL' obj
    , return $ emit " "
    , prettyPrintIL' sts
    ]
  match (IfElse _ (Binary _ EqualTo cond@Binary{} (BooleanLiteral Nothing True)) thens elses) = mconcat <$> sequence
    [ return $ emit "if "
    , prettyPrintIL' cond
    , return $ emit " "
    , prettyPrintIL' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
    ]
  match (IfElse _ (Binary _ EqualTo cond@Binary{} (BooleanLiteral Nothing False)) thens elses) = mconcat <$> sequence
    [ return $ emit "if !("
    , prettyPrintIL' cond
    , return $ emit ") "
    , prettyPrintIL' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
    ]
  match (IfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if "
    , prettyPrintIL' cond
    , return $ emit " "
    , prettyPrintIL' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintIL') elses
    ]
  match (Return _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintIL' value
    ]
  match (ReturnNoResult _) = return . emit $ "return " <> undefinedName
  -- match (Throw _ _) = return mempty
  match (Throw _ value) = mconcat <$> sequence
    [ return $ emit "panic("
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

isComplexLiteral :: AST -> Bool
-- isComplexLiteral Function{} = True
isComplexLiteral ObjectLiteral{} = True
isComplexLiteral ArrayLiteral{} = True
isComplexLiteral _ = False

unApp :: AST -> [AST] -> (AST, [AST])
unApp (App _ val [arg]) args = unApp val (arg : args)
unApp other args = (other, args)

prettyStatements :: (Emit gen) => [AST] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  ils <- forM sts prettyPrintIL'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map (indentString <>) ils

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
    OperatorTable [ [ unary New "?" ]
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
stringLiteral pss | Just s <- decodeString pss = stringLiteral' s
  where
  stringLiteral' :: Text -> Text
  stringLiteral' s = "\"" <> T.concatMap encodeChar s <> "\""
  encodeChar :: Char -> Text
  encodeChar '\0' = "\\x00"
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

interfaceSource :: Text -> [(Text,Bool)] -> [Ident] -> Text
interfaceSource _ _ _ = ""

implHeaderSource :: Text -> [Text] -> Text -> Text
implHeaderSource mn imports otherPrefix =
  "// Code generated by psgo. DO NOT EDIT.\n\n" <>
  "package " <> (if mn == "Main" then "main" else mn) <> "\n\n" <>
  "import . \"" <> runtime <> "\"\n" <>
  (if mn == "Main"
      then "import _ \"" <> otherPrefix <> "/" <> ffiLoader <> "\"\n"
      else "\n") <>
  "import (\n" <>
  (T.concat $ formatImport <$> imports) <> ")\n\n" <>
  "type _ = Any\n\n"
  where
  formatImport :: Text -> Text
  formatImport s = "    \"" <> otherPrefix <> "/" <> modPrefix <> "/" <> s <> "\"\n"

implFooterSource :: Text -> [Ident] -> Text
implFooterSource mn foreigns =
  "\n\n\n" <>
  (if null foreigns
    then ""
    else ("// Foreign values\n\n" <>
          "var " <> foreignDict <> " = "<> foreignMod <> "(\"" <> mn <> "\")\n\n" <>
          (T.concat $ (\foreign' ->
                        let name = moduleIdentToIL foreign' in
                        varDecl <> " " <> initName name <> " Once\n" <>     
                        varDecl <> " " <> valueName name <> " " <> anyType <> "\n\n" <>
                        "func " <>
                        withPrefix name <>
                        "() Any { \n" <>
                        "    " <> initName name <> ".Do(func() {\n" <>
                        "        " <> valueName name <> " = " <>
                                        "Get(" <> foreignDict <> ", " <>
                                            (stringLiteral . mkString $ runIdent foreign') <> ")\n" <>
                        "    })\n" <>
                        "    return " <> valueName name <> "\n" <> 
                        "}\n\n") <$> foreigns))) <>
  if mn == "Main" then mainSource else "\n"
  where
  mainSource :: Text
  mainSource = "\
    \func main() {\n\
    \    Run(PS__main())\n\
    \}\n\n\
    \"

varDecl :: Text
varDecl = "var"

renderArg :: Text -> Text -> Text
renderArg decl arg = arg <> " " <> decl

foreignMod :: Text
foreignMod = "Foreign"

foreignDict :: Text
foreignDict = "foreign"

modLabel :: Text
modLabel = "purescript-native"

modPrefix :: Text
modPrefix = modLabel <> "/output"

runtime :: Text
runtime = "github.com/" <> modLabel <> "/go-runtime"

ffiLoader :: Text
ffiLoader = modLabel <> "/ffi-loader"

initName :: Text -> Text
initName s = "ₒ" <> s

valueName :: Text -> Text
valueName s = "ₐ" <> s
