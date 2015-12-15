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
{-# LANGUAGE CPP #-}

module Language.PureScript.Pretty.Cpp (
    dotsTo,
    linebreak,
    prettyPrintCpp,
) where

import Data.Bits
import Data.Char
import Data.List
import Data.Maybe (catMaybes, fromMaybe)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((<+>))
import Control.Monad.State
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Comments
import Language.PureScript.Pretty.Common
import qualified Language.PureScript.Constants as C

import Numeric

-- import Debug.Trace

literals :: Pattern PrinterState Cpp String
literals = mkPattern' match
  where
  match :: Cpp -> StateT PrinterState Maybe String
  match CppNoOp = return []
  match CppEndOfHeader = return []
  match (CppNumericLiteral (Left n)) = return $ show n ++ "L"
  match (CppNumericLiteral n) = return $ either show show n
  match (CppStringLiteral s) | all isAscii s = return $ string s
  match (CppStringLiteral s) = return $ "u8" ++ string s
  match (CppCharLiteral c) = return $ show c
  match (CppBooleanLiteral True) = return "true"
  match (CppBooleanLiteral False) = return "false"
  match (CppArrayLiteral [x@CppArrayLiteral {}]) = -- Works around what appears to be a recent clang bug
    fmap concat $ sequence
    [ return $ runType arrayType
    , return "{ { "
    , prettyPrintCpp' x
    , return " } }"
    ]
  match (CppArrayLiteral xs) = fmap concat $ sequence
    [ return $ runType arrayType
    , return "{ "
    , fmap (intercalate ", ") $ forM xs prettyPrintCpp'
    , return " }"
    ]
  match (CppObjectLiteral ps) = fmap concat $ sequence
    [ return $ runType mapType ++ "{\n"
    , withIndent $ do
        cpps <- forM ps $ \(key, value) -> do
                            val <- prettyPrintCpp' value
                            return $ "{ KEY" ++ parens(show key) ++ ", " ++ val ++ " }"
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) cpps
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (CppFunction name args rty qs ret) =
    fmap concat $ sequence
    [ return . concatMap (++ " ") . filter (not . null) $ runValueQual <$> qs
    , return "auto "
    , return name
    , return $ parens (intercalate ", " $ argstr <$> args)
    , return (maybe "" ((" -> " ++) . runType) rty)
    , if ret /= CppNoOp
        then do
          cpps <- prettyPrintCpp' ret
          return $ ' ' : cpps
        else return ""
    ]
  match (CppBlock sts) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return $ if null sts then "" else "\n"
    , currentIndent
    , return "}"
    ]
  match (CppNamespace _ []) = return []
  match (CppNamespace (':':':':name) sts) = fmap concat $ sequence $
    [ return "\n"
    , currentIndent
    , return $ "namespace " ++ (dotsTo '_' name) ++ " {\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (CppNamespace name sts) = fmap concat $ sequence $
    [ return "\n"
    , currentIndent
    , return $ "namespace " ++ (dotsTo '_' name) ++ " {\n"
    , withIndent $ prettyStatements sts'
    , return "\n"
    , currentIndent
    , return "}"
    ]
    ++ let (cpp', cpps') = fromNested nested' in
       map match cpp'
    ++ if null cpps' then [] else [match (CppNamespace name (filter isUseNamespace sts ++ cpps'))]
    where
    (sts', nested') = break isNestedNamespace sts
    fromNested :: [Cpp] -> ([Cpp], [Cpp])
    fromNested [] = ([],[])
    fromNested cpps@((CppNamespace nm _):_) = ([foldl1 combineNamespaces namespaces], others)
      where
      (namespaces, others) = span inSameNamespace cpps
      inSameNamespace :: Cpp -> Bool
      inSameNamespace (CppNamespace nm' _) | nm' == nm = True
      inSameNamespace _ = False
    fromNested _ = error "Not a nested namespace"
    combineNamespaces :: Cpp -> Cpp -> Cpp
    combineNamespaces (CppNamespace nm ss) (CppNamespace nm' ss')
      | nm == nm' = CppNamespace nm (ss ++ filter (not . isUseNamespace) ss')
    combineNamespaces _ _ = error "Cannot fold cpps"
    isUseNamespace :: Cpp -> Bool
    isUseNamespace (CppUseNamespace{}) = True
    isUseNamespace _ = False
  match (CppInclude path name) =
    let fullpath
          | null path = last . words . dotsTo ' ' $ name
          | otherwise = (dotsTo '/' path) ++ '/' : (last . words . dotsTo ' ' $ name) in
    fmap concat $ sequence
    [ return $ "#include \"" ++ fullpath ++ ".hh\""
    ]
  match (CppUseNamespace name) = fmap concat $ sequence
    [ return $ "using namespace " ++ (dotsTo '_' name)
    ]
  match (CppTypeAlias (newName, newTyps) typ spec) =
    let typ' = runType typ
        (tmps, name') = if null newTyps
                          then ([], typ')
                          else ([return (templDecl newTyps), return "\n", currentIndent], typ')
    in fmap concat $ sequence $
    tmps ++
    [ return $ "using "
    , return newName
    , return " = "
    , return $ if null spec then name' else spec ++ angles name'
    ]
  match (CppCast typ val) = return $
    case val of
      CppNumericLiteral {} -> vstr
      CppStringLiteral {}  -> vstr
      CppBooleanLiteral {} -> vstr
      _ -> val' ++ ".cast" ++ angles (runType typ) ++ parens []
    where
    vstr = prettyPrintCpp1 val
    val' | '(' `elem` vstr || '[' `elem` vstr = parens vstr
         | otherwise = vstr

  match (CppVar ident) = return ident
  match (CppApp v [CppNoOp]) = return (prettyPrintCpp1 v)
  match (CppVariableIntroduction (ident, _) _ (Just value))
    | ident == C.__unused = fmap concat $ sequence
    [ prettyPrintCpp' value
    ]
  match (CppVariableIntroduction (ident, typ) qs value) =
    fmap concat $ sequence
    [ return . concatMap (++ " ") . filter (not . null) $ runValueQual <$> qs
    , return (maybe "any" runType typ ++ " ")
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintCpp') value
    ]
  match (CppAssignment target value) = fmap concat $ sequence
    [ prettyPrintCpp' target
    , return " = "
    , prettyPrintCpp' value
    ]
  match (CppWhile cond sts) = fmap concat $ sequence
    [ return "while ("
    , prettyPrintCpp' cond
    , return ") "
    , prettyPrintCpp' sts
    ]
  match (CppIfElse cond thens elses) = fmap concat $ sequence
    [ return "if ("
    , prettyPrintCpp' cond
    , return ") "
    , prettyPrintCpp' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintCpp') elses
    ]
  match (CppReturn (CppBlock (cpp:cpps))) = fmap concat $ sequence
    [   do s <- prettyPrintCpp' cpp
           return $ dropWhile isSpace s
      , return $ if null cpps then "" else "\n"
      , prettyStatements cpps
    ]
  match (CppReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintCpp' value
    ]
  match (CppThrow value) = fmap concat $ sequence
    [ return "throw "
    , prettyPrintCpp' value
    ]
  match CppContinue = return "continue;"
  match (CppComment [] cpp) = match cpp
  match (CppComment com cpp) = fmap concat $ sequence $
    [ return "\n"
    , currentIndent
    , return "/**\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return " */\n"
    , currentIndent
    , prettyPrintCpp' cpp
    ]
    where
    commentLines :: Comment -> [String]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = lines s

    asLine :: String -> StateT PrinterState Maybe String
    asLine s = do
      i <- currentIndent
      return $ i ++ " * " ++ removeComments s ++ "\n"

    removeComments :: String -> String
    removeComments ('*' : '/' : s) = removeComments s
    removeComments (c : s) = c : removeComments s

    removeComments [] = []
  match (CppRaw cpp) = return cpp
  match _ = mzero

string :: String -> String
string s = '"' : concatMap encodeChar (decodeSurrogates s) ++ "\""
  where
  decodeSurrogates :: String -> String
  decodeSurrogates [c] = [c]
  decodeSurrogates s = catMaybes $ zipWith decodePair s (tail s ++ ['\0'])
    where
    decodePair :: Char -> Char -> Maybe Char
    decodePair a b
      | fromEnum a >= 0xD800 && fromEnum a <= 0xDFFF &&
        fromEnum b >= 0xD800 && fromEnum b <= 0xDFFF
      = Just . chr $ 0x10000 + shiftL (fromEnum a .&. 0x03FF) 10 + (fromEnum b .&. 0x03FF)
    decodePair a _
      | fromEnum a >= 0xD800 && fromEnum a <= 0xDFFF  = Nothing
    decodePair a _  = Just a
  encodeChar :: Char -> String
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c | fromEnum c > 0xFFFF = "\\U" ++ leading ++ hex
    where
    hex = showHex (fromEnum c) ""
    leading = replicate (8 - length hex) '0'
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c < 0x10 = "\\x0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0x7E || fromEnum c < 0x20 = "\\x" ++ showHex (fromEnum c) ""
  encodeChar c = [c]

accessor :: Pattern PrinterState Cpp (String, Cpp)
accessor = mkPattern match
  where
  match (CppAccessor prop val) = Just (prettyPrintCpp1 prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState Cpp (String, Cpp)
indexer = mkPattern' match
  where
  match (CppIndexer index@(CppNumericLiteral {}) val) = (,) <$> prettyPrintCpp' index <*> pure val
  match (CppIndexer (CppStringLiteral index) val) = return ("KEY" ++ parens (show index), val)
  match _ = mzero

lam :: Pattern PrinterState Cpp ((String, [(String, Maybe CppType)], Maybe CppType), Cpp)
lam = mkPattern match
  where
  match (CppLambda caps args rty ret) =
    Just ((concatMap runCaptureType caps, args, rty), ret)
  match _ = Nothing

app :: Pattern PrinterState Cpp (String, Cpp)
app = mkPattern' match
  where
  match (CppApp _ [CppNoOp]) = mzero
  match (CppApp val args) = do
    cpps <- mapM prettyPrintCpp' args
    return (intercalate ", " cpps, val)
  match _ = mzero

unary' :: CppUnaryOp -> (Cpp -> String) -> Operator PrinterState Cpp String
unary' op mkStr = Wrap match (++)
  where
  match :: Pattern PrinterState Cpp (String, Cpp)
  match = mkPattern match'
    where
    match' (CppUnary op' val) | op' == op = Just (mkStr val, val)
    match' _ = Nothing

unary :: CppUnaryOp -> String -> Operator PrinterState Cpp String
unary op str = unary' op (const str)

negateOperator :: Operator PrinterState Cpp String
negateOperator = unary' CppNegate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (CppUnary CppNegate _) = True
  isNegate _ = False

binary :: BinaryOp -> String -> Operator PrinterState Cpp String
binary op str = AssocL match (\v1 v2 -> v1 ++ str ++ v2)
  where
  match :: Pattern PrinterState Cpp (Cpp, Cpp)
  match = mkPattern match'
    where
    match' (CppBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [Cpp] -> StateT PrinterState Maybe String
prettyStatements sts = do
  cpps <- forM (filter (not . isNoOp) sts) prettyPrintCpp'
  indentString <- currentIndent
  return $ intercalate "\n" $ map (addsemi . (indentString ++)) cpps
  where
  addsemi :: String -> String
  addsemi [] = []
  addsemi s | all isSpace s = []
  addsemi s@('#':_) = s
  addsemi s = s ++ ";"

-- |
-- Generate a pretty-printed string representing a C++11 expression
--
prettyPrintCpp1 :: Cpp -> String
prettyPrintCpp1 = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintCpp'

-- |
-- Generate a pretty-printed string representing a collection of C++11 expressions at the same indentation level
--
prettyPrintCpp :: [Cpp] -> String
prettyPrintCpp = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

-- |
-- Generate an indented, pretty-printed string representing a C++11 expression
--
prettyPrintCpp' :: Cpp -> StateT PrinterState Maybe String
prettyPrintCpp' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState Cpp String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState Cpp String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "::" ++ prop ]
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args val -> val ++ parens args ]
                  , [ unary CppNew "new " ]
                  , [ Wrap lam $ \(caps, args, rty) ret -> '[' : caps ++ "]"
                        ++ let args' = argstr <$> args in
                           parens (intercalate ", " args')
                        ++ maybe "" ((" -> " ++) . runType) rty
                        ++ " "
                        ++ ret ]
                  , [ unary     CppNot                "!"
                    , unary     CppBitwiseNot         "~"
                    , unary     CppPositive           "+"
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
                    , binary    LessThanOrEqual      " <= "
                    , binary    GreaterThan          " > "
                    , binary    GreaterThanOrEqual   " >= "
                    , binary    Equal                " == "
                    , binary    NotEqual             " != " ]
                  , [ binary    BitwiseAnd           " & " ]
                  , [ binary    BitwiseXor           " ^ " ]
                  , [ binary    BitwiseOr            " | " ]
                  , [ binary    And                  " && " ]
                  , [ binary    Or                   " || " ]
                    ]

dotsTo :: Char -> String -> String
dotsTo chr' = map (\c -> if c == '.' then chr' else c)

argstr :: (String, Maybe CppType) -> String
argstr (name, Nothing) = argStr name CppAuto
argstr (name, Just typ) | name == C.__unused = argStr [] typ
argstr (name, Just typ) = argStr name typ

argStr :: String -> CppType -> String
argStr name typ = runType typ ++ if null name then [] else " " ++ name

templDecl :: [(String, Int)] -> String
templDecl ps = "template " ++ angles (intercalate ", " (go <$> ps))
  where
  go :: (String, Int) -> String
  go (name, 0) = "typename " ++ name
  go (name, n) = "typename" ++ parens (intercalate "," $ replicate n "typename") ++ ' ' : name

isNestedNamespace :: Cpp -> Bool
isNestedNamespace (CppNamespace (':':':':_) _) = True
isNestedNamespace _ = False

angles :: String -> String
angles s = '<' : s ++ ">"

linebreak :: [Cpp]
linebreak = [CppRaw ""]

isNoOp :: Cpp -> Bool
isNoOp CppNoOp = True
isNoOp (CppComment [] CppNoOp) = True
isNoOp _ = False
