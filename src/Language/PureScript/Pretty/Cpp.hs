-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Cpp
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for the C++11 AST
--
-----------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Pretty.Cpp
  ( dotsTo
  , linebreak
  , prettyPrintCpp
  ) where

import Prelude.Compat

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((<+>))
import qualified Control.Arrow as A
import Control.Monad.State
import Numeric (showHex)
import Control.PatternArrows

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Comments
import qualified Language.PureScript.Constants as C
import Language.PureScript.Pretty.Common (PrinterState(..), intercalate, parensT)

-- |
-- Get the current indentation level
--
currentIndent :: StateT PrinterState Maybe Text
currentIndent = do
  current <- get
  return $ T.replicate (indent current) " "

blockIndent :: Int
blockIndent = 2

-- |
-- Pretty print with a new indentation level
--
withIndent :: StateT PrinterState Maybe Text -> StateT PrinterState Maybe Text
withIndent action = do
  modify $ \st -> st { indent = indent st + blockIndent }
  result <- action
  modify $ \st -> st { indent = indent st - blockIndent }
  return result

show' :: Show a => a -> Text
show' = T.pack . show

---------------------------------------------------------------------------------------------------
literals :: Pattern PrinterState Cpp Text
---------------------------------------------------------------------------------------------------
literals = mkPattern' match
  where
  match :: Cpp -> StateT PrinterState Maybe Text
  match CppNoOp = return ""
  match CppEndOfHeader = return ""
  match (CppNumericLiteral (Left n)) = return $ show' n
  match (CppNumericLiteral n) = return $ either show' show' n
  match (CppStringLiteral s)
    | T.all isAscii s = return $ string s
  match (CppStringLiteral s) = return $ "u8" <> string s
  match (CppCharLiteral c) = return $ "'" <> T.singleton c <> "'"
  match (CppBooleanLiteral True) = return "true"
  match (CppBooleanLiteral False) = return "false"
  match (CppArrayLiteral [x@CppArrayLiteral {}]) = -- Works around what appears to be a recent clang bug
    mconcat <$> sequence
    [ return $ runType arrayType
    , return "{ { "
    , prettyPrintCpp' x
    , return " } }"
    ]
  match (CppArrayLiteral xs) = mconcat <$> sequence
    [ return $ runType arrayType
    , return "{ "
    , fmap (T.intercalate ", ") $ forM xs prettyPrintCpp'
    , return " }"
    ]
  match (CppDataLiteral xs) = mconcat <$> sequence
    [ return $ runType (dataType $ length xs)
    , return "{ "
    , fmap (T.intercalate ", ") $ forM xs prettyPrintCpp'
    , return " }"
    ]
  match (CppEnum name ty es) = mconcat <$> sequence
    [ return "enum"
    , return $ maybe "" (" " <>) name
    , return $ maybe "" ((" : " <>) . runType) ty
    , return " { "
    , return $ T.intercalate ", " es
    , return " }"
    ]
  match (CppMapLiteral _ ps) = mconcat <$> sequence
    [ return $ runType (mapType $ length ps + 1) <> "{{\n"
    , withIndent $ do
        cpps <-
          forM ps $ \(key, value) -> do
            value' <- prettyPrintCpp' value
            key' <- prettyPrintCpp' key
            return $ "{ " <> key' <> ", " <> value' <> " }"
        indentString <- currentIndent
        return $ T.intercalate ", \n" $ map (indentString <>) (cpps ++ ["{ nullptr, nullptr }"])
    , return "\n"
    , currentIndent
    , return "}}"
    ]
  match (CppFunction name args rty qs ret) =  mconcat <$> sequence
    [ return . T.concat . fmap (<> " ") . filter (not . T.null) $ runValueQual <$> qs
    , return "auto "
    , return name
    , return $ parensT (T.intercalate ", " $ argstr <$> args)
    , return (maybe "" ((" -> " <>) . runType) rty)
    , if ret /= CppNoOp
        then do
          cpps <- prettyPrintCpp' ret
          return $ ' ' `T.cons` cpps
        else return ""
    ]
  match (CppBlock sts) = mconcat <$> sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return $ if null sts then "" else "\n"
    , currentIndent
    , return "}"
    ]
  match (CppNamespace _ []) = return ""
  match (CppNamespace name sts) | T.null name = mconcat <$> sequence
    [ return "\n"
    , prettyStatements sts
    , return "#"
    ]
  match (CppNamespace name sts)
    | Just name' <- T.stripPrefix "::" name = mconcat <$> sequence
    [ return "\n"
    , currentIndent
    , return $ "namespace " <> (dotsTo '_' name') <> " {\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}#"
    ]
  match (CppNamespace name sts) = mconcat <$> sequence (
    [ return "\n"
    , currentIndent
    , return $ "namespace " <> (dotsTo '_' name) <> " {\n"
    , withIndent $ prettyStatements sts'
    , return "\n"
    , currentIndent
    , return "}#"
    ] <>
    let (cpp', cpps') = fromNested nested'
    in map match cpp' ++
       if null cpps'
         then []
         else [match (CppNamespace name (filter isUseNamespace sts <> cpps'))]
    )
    where
    (sts', nested') = break isNestedNamespace sts
    fromNested :: [Cpp] -> ([Cpp], [Cpp])
    fromNested [] = ([], [])
    fromNested cpps@((CppNamespace nm _):_) = ([foldl1 combineNamespaces namespaces], others)
      where
      (namespaces, others) = span inSameNamespace cpps
      inSameNamespace :: Cpp -> Bool
      inSameNamespace (CppNamespace nm' _)
        | nm' == nm = True
      inSameNamespace _ = False
    fromNested _ = error "Not a nested namespace"
    combineNamespaces :: Cpp -> Cpp -> Cpp
    combineNamespaces (CppNamespace nm ss) (CppNamespace nm' ss')
      | nm == nm' = CppNamespace nm (ss ++ filter (not . isUseNamespace) ss')
    combineNamespaces _ _ = error "Cannot fold cpps"
    isUseNamespace :: Cpp -> Bool
    isUseNamespace (CppUseNamespace {}) = True
    isUseNamespace _ = False
  match (CppStruct (name) []) = return $ "struct " <> name <> " {}"
  match (CppStruct (name) mems) = mconcat <$> sequence
    [ return "\n"
    , currentIndent
    , return $ "struct " <> name <> " {\n"
    , withIndent $ prettyStatements mems
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (CppInclude path name) =
    let fullpath
          | T.null path = last . T.words . dotsTo ' ' $ name
          | otherwise = path <> "/" <> (last . T.words . dotsTo ' ' $ name) in
    mconcat <$> sequence
    [ return $ "#include \"" <> fullpath <> ".hh\""
    ]
  match (CppUseNamespace name) = mconcat <$> sequence
    [ return $ "using namespace " <> (dotsTo '_' name)
    ]
  match (CppTypeAlias (newName, newTyps) typ spec) =
    let typ' = runType typ
        (tmps, name') = if null newTyps
                          then ([], typ')
                          else ([return (templDecl newTyps), return "\n", currentIndent], typ')
    in mconcat <$> sequence (
    tmps ++
    [ return "using "
    , return newName
    , return " = "
    , return $ if T.null spec then name' else spec <> angles name'
    ])
  match (CppCast typ val) = return $
    case val of
      CppNumericLiteral {} -> vstr
      CppStringLiteral {} -> vstr
      CppBooleanLiteral {} -> vstr
      _ -> "cast" <> angles (runType typ) <> parensT val'
    where
    vstr = prettyPrintCpp1 val
    val' | "(" `T.isInfixOf` vstr || "[" `T.isInfixOf` vstr = parensT vstr
         | otherwise = vstr
  match (CppGet i@(CppSymbol _) val) =
    return . prettyPrintCpp1 $ CppApp (CppVar "get") [i, val]
  match (CppGet i val) =
    return . prettyPrintCpp1 $ CppApp (CppVar $ "get" <> angles (prettyPrintCpp1 i)) [val]
  match (CppVar ident)
    | ident == C.__unused = match (CppVar $ '$' `T.cons` ident)
  match (CppVar ident) = return ident
  match (CppSymbol ident) = return $ "SYMBOL" <> parensT (symbolname ident)
  match (CppApp v [CppNoOp]) = return (prettyPrintCpp1 v)
  match (CppVariableIntroduction (ident, typ) qs value)
    | ident == C.__unused = match (CppVariableIntroduction ('$' `T.cons` ident, typ) qs value)
  match (CppVariableIntroduction (ident, typ) qs value) =
    mconcat <$> sequence
    [ return . T.concat . fmap (<> " ") . filter (not . T.null) $ runValueQual <$> qs
    , return $ runType (fromMaybe (Any [Const]) typ) <> " "
    , return ident
    , maybe (return "") (fmap (" = " <>) . prettyPrintCpp') value
    ]
  match (CppAssignment target value) = mconcat <$> sequence
    [ prettyPrintCpp' target
    , return " = "
    , prettyPrintCpp' value
    ]
  match (CppWhile cond sts) = mconcat <$> sequence
    [ return "while ("
    , prettyPrintCpp' cond
    , return ") "
    , prettyPrintCpp' sts'
    ]
    where
    sts' | CppBlock cpps'@(_:_) <- sts,
           last cpps' == CppContinue = CppBlock (init cpps')
         | otherwise = sts
  match (CppIfElse cond thens elses) = mconcat <$> sequence
    [ return "if ("
    , prettyPrintCpp' cond
    , return ") "
    , prettyPrintCpp' thens
    , maybe (return "") (fmap (" else " <>) . prettyPrintCpp') elses
    ]
  match (CppSwitch cond cases@((case1,_):_:_)) = mconcat <$> sequence
    [ return "switch "
    , return . parensT $ prettyPrintCpp1 (cast case1 cond)
    , return " {\n"
    , withIndent $ do
        cpps <- forM cases $ \(c, s) -> do
                              c' <- prettyPrintCpp' c
                              s' <- prettyPrintCpp' s
                              return $ "case " <> c' <> ": " <> s' <> ";"
        indentString <- currentIndent
        return $ T.intercalate "\n" $ map (indentString <>) cpps
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    cast :: Cpp -> Cpp -> Cpp
    cast CppNumericLiteral {} cpp' = CppCast intType cpp'
    cast CppBooleanLiteral {} cpp' = CppCast boolType cpp'
    cast CppCharLiteral {} cpp' = CppCast charType cpp'
    cast _ cpp' = cpp'
  match (CppReturn (CppBlock (cpp:cpps))) = mconcat <$> sequence
    [   do s <- prettyPrintCpp' cpp
           return $ T.dropWhile isSpace s
      , return $ if null cpps then "" else "\n"
      , prettyStatements cpps
    ]
  match (CppReturn value@(CppStringLiteral _)) = mconcat <$> sequence
    [ return "return "
    , return . runType $ Any []
    , return "("
    , prettyPrintCpp' value
    , return ")"
    ]
  match (CppReturn value) = mconcat <$> sequence
    [ return "return "
    , prettyPrintCpp' value
    ]
  match (CppThrow value) = mconcat <$> sequence
    [ return "throw "
    , prettyPrintCpp' value
    ]
  match CppContinue = return "continue"
  match (CppComment [] cpp) = match cpp
  match (CppComment com cpp) = mconcat <$> sequence
   ([ return "\n"
    , currentIndent
    , return "/**\n"
    ] <>
    map asLine (concat $ commentLines <$> com) ++
    [ currentIndent
    , return " */\n"
    , currentIndent
    , prettyPrintCpp' cpp
    ])
    where
    commentLines :: Comment -> [Text]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = T.lines s
    asLine :: Text -> StateT PrinterState Maybe Text
    asLine s = do
      i <- currentIndent
      return $ i <> " * " <> removeComments s <> "\n"

    removeComments :: Text -> Text
    removeComments t =
      case T.stripPrefix "*/" t of
        Just rest -> removeComments rest
        Nothing -> case T.uncons t of
          Just (x, xs) -> x `T.cons` removeComments xs
          Nothing -> ""
  match (CppRaw cpp) = return cpp
  match _ = mzero

string :: Text -> Text
string s = "\"" <> T.concatMap encodeChar s <> "\""
  where
  encodeChar :: Char -> Text
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  -- PureScript strings are sequences of UTF-16 code units, so this case should never be hit.
  -- If it is somehow hit, though, output the designated Unicode replacement character U+FFFD.
  encodeChar c | fromEnum c > 0xFFFF = "\\uFFFD"
  encodeChar c | fromEnum c > 0xFFF = "\\u" <> showHex' (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" <> showHex' (fromEnum c) ""
  encodeChar c | fromEnum c < 0x10 = "\\x0" <> showHex' (fromEnum c) ""
  encodeChar c | fromEnum c > 0x7E || fromEnum c < 0x20 = "\\x" <> showHex' (fromEnum c) ""
  encodeChar c = T.singleton c

  showHex' a b = T.pack (showHex a b)

accessor :: Pattern PrinterState Cpp (Text, Cpp)
accessor = mkPattern match
  where
  match (CppAccessor prop val) = Just (prettyPrintCpp1 prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState Cpp (Text, Cpp)
indexer = mkPattern' match
  where
  match (CppIndexer (CppNumericLiteral (Left 0)) val) = return ("0UL", val)
  match (CppIndexer (CppNumericLiteral (Left index)) val) = return (show' index, val)
  match (CppIndexer index val) = do
    index' <- prettyPrintCpp' index
    return (index', val)
  match _ = mzero

lam :: Pattern PrinterState Cpp ((Text, [(Text, Maybe CppType)], Maybe CppType), Cpp)
lam = mkPattern match
  where
  match (CppLambda caps args rty ret) = Just ((T.concat $ runCaptureType <$> caps, args, rty), ret)
  match _ = Nothing

app :: Pattern PrinterState Cpp (Text, Cpp)
app = mkPattern' match
  where
  match (CppApp _ [CppNoOp]) = mzero
  match (CppApp val args) = do
    cpps <- mapM prettyPrintCpp' args
    return (T.intercalate ", " cpps, val)
  match _ = mzero

unary' :: UnaryOperator -> (Cpp -> Text) -> Operator PrinterState Cpp Text
unary' op mkStr = Wrap match (<>)
  where
  match :: Pattern PrinterState Cpp (Text, Cpp)
  match = mkPattern match'
    where
    match' (CppUnary op' val)
      | op' == op = Just (mkStr val, val)
    match' _ = Nothing

unary :: UnaryOperator -> Text -> Operator PrinterState Cpp Text
unary op str = unary' op (const str)

negateOperator :: Operator PrinterState Cpp Text
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (CppUnary Negate _) = True
  isNegate _ = False

binary :: BinaryOperator -> Text -> Operator PrinterState Cpp Text
binary op str = AssocL match (\v1 v2 -> v1 <> str <> v2)
  where
  match :: Pattern PrinterState Cpp (Cpp, Cpp)
  match = mkPattern match'
    where
    match' (CppBinary op' v1@(CppStringLiteral _) v2@(CppStringLiteral _))
      | op' == op = Just (CppApp (CppVar "std::string") [v1], v2)
    match' (CppBinary op' v1 v2)
      | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [Cpp] -> StateT PrinterState Maybe Text
prettyStatements sts = do
  cpps <- forM (filter (not . isNoOp) sts) prettyPrintCpp'
  indentString <- currentIndent
  return $ T.intercalate "\n" $ map (addsemi . (indentString <>)) cpps
  where
  addsemi :: Text -> Text
  addsemi s | T.null s = s
  addsemi s
    | T.all isSpace s = ""
  addsemi s
    | T.head s == '#' = s
  addsemi s
    | T.last s == '#' = T.init s
  addsemi s = s <> ";"

-- |
-- Generate a pretty-printed string representing a C++11 expression
--
prettyPrintCpp1 :: Cpp -> Text
prettyPrintCpp1 = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintCpp'

-- |
-- Generate a pretty-printed string representing a collection of C++11 expressions at the same indentation level
--
prettyPrintCpp :: [Cpp] -> Text
prettyPrintCpp = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

-- |
-- Generate an indented, pretty-printed string representing a C++11 expression
--
prettyPrintCpp' :: Cpp -> StateT PrinterState Maybe Text
prettyPrintCpp' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState Cpp Text
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensT matchValue)
  operators :: OperatorTable PrinterState Cpp Text
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val <> "::" <> prop ]
                  , [ Wrap indexer $ \index val -> val <> "[" <> index <> "]" ]
                  , [ Wrap app $ \args val -> val <> parensT args ]
                  , [ unary New "new " ]
                  , [ Wrap lam $ \(caps, args, rty) ret -> '[' `T.cons` caps <> "]"
                        <> let args' = argstr <$> args in
                           parensT (T.intercalate ", " args')
                        <> maybe "" ((" -> " <>) . runType) rty
                        <> " "
                        <> ret ]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             " * "
                    , binary    Divide               " / "
                    , binary    Modulus              " % "
                    , binary    Dot                  "." ]
                  , [ binary    Add                  " + "
                    , binary    Subtract             " - " ]
                  , [ binary    ShiftLeft            " << "
                    , binary    ShiftRight           " >> " ]
                  , [ binary    LessThan             " < "
                    , binary    LessThanOrEqualTo    " <= "
                    , binary    GreaterThan          " > "
                    , binary    GreaterThanOrEqualTo " >= "
                    , binary    EqualTo                " == "
                    , binary    NotEqualTo             " != " ]
                  , [ binary    BitwiseAnd           " & " ]
                  , [ binary    BitwiseXor           " ^ " ]
                  , [ binary    BitwiseOr            " | " ]
                  , [ binary    And                  " && " ]
                  , [ binary    Or                   " || " ]
                    ]

dotsTo :: Char -> Text -> Text
dotsTo chr' = T.map (\c -> if c == '.' then chr' else c)

argstr :: (Text, Maybe CppType) -> Text
argstr (name, Nothing) = argStr name (Auto [])
argstr (name, Just typ)
  | name == C.__unused = argStr "" typ
argstr (name, Just typ) = argStr name typ

argStr :: Text -> CppType -> Text
argStr name typ = runType typ <>
  if T.null name
    then ""
    else " " <> name

templDecl :: [(Text, Int)] -> Text
templDecl ps = "template " <> angles (intercalate ", " (go <$> ps))
  where
  go :: (Text, Int) -> Text
  go (name, 0) = "typename " <> name
  go (name, n) = "typename" <> parensT (intercalate "," $ replicate n "typename") <> (' ' `T.cons` name)

isNestedNamespace :: Cpp -> Bool
isNestedNamespace (CppNamespace name _)
  | "::" `T.isPrefixOf` name = True
isNestedNamespace _ = False

angles :: Text -> Text
angles s = "<" <> s <> ">"

linebreak :: [Cpp]
linebreak = [CppRaw ""]

isNoOp :: Cpp -> Bool
isNoOp CppNoOp = True
isNoOp (CppComment [] CppNoOp) = True
isNoOp _ = False
