-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Cpp
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for the C++11 AST
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Pretty.Cpp (
    prettyPrintCpp
) where

import Data.List
import Data.Maybe (fromMaybe)

import Control.Applicative
import Control.Arrow ((<+>))
import Control.Monad.State
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Comments
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Pretty.Common

import Numeric

import Debug.Trace

literals :: Pattern PrinterState Cpp String
literals = mkPattern' match
  where
  match :: Cpp -> StateT PrinterState Maybe String
  match CppNoOp = return []
  match (CppNumericLiteral n) = return $ either show show n
  match (CppStringLiteral s) = return $ string s
  match (CppBooleanLiteral True) = return "true"
  match (CppBooleanLiteral False) = return "false"
  match (CppArrayLiteral xs) = fmap concat $ sequence
    [ return "[ "
    , fmap (intercalate ", ") $ forM xs prettyPrintCpp'
    , return " ]"
    ]
  match (CppObjectLiteral []) = return "{}"
  match (CppObjectLiteral ps) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ do
        cpps <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key ++ ": ") ++) . prettyPrintCpp' $ value
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) cpps
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    objectPropertyToString :: String -> String
    objectPropertyToString s | identNeedsEscaping s = show s
                             | otherwise = s
  match (CppBlock sts) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (CppNamespace name sts) = fmap concat $ sequence
    [ return "\n"
    , currentIndent
    , return $ "namespace " ++ name ++ " {\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (CppInclude name) =
    let fullpath = (dotsTo '/' name) ++ '/' : (last . words . dotsTo ' ' $ name) in
    fmap concat $ sequence
    [ return $ "#include \"" ++ fullpath ++ ".hh\""
    ]
  match (CppUseNamespace name) = fmap concat $ sequence
    [ return $ "using namespace " ++ (dotsTo '_' name) ++ ";"
    ]
  match (CppStruct (name, params) supers cms _) = fmap concat $ sequence
    [ return "\n"
    , currentIndent
    , return (templDecl' params)
    , return "\n"
    , currentIndent
    , return $ "struct " ++ classstr (name, either (const []) id params)
    , return $ if null supers then
                 []
               else
                 " : public " ++ intercalate ", public " (classstr <$> supers)
    , return " {\n"
    , withIndent $ prettyStatements cms
    , return "\n"
    , currentIndent
    , return "};"
    ]
  match (CppStructValue name []) =
    return ("make_data" ++ "<_" ++ name ++ "_>")
  match (CppStructValue name typs) =
    return ("make_data" ++ "<_" ++ name ++ "_<" ++ intercalate "," typs ++ ">>")
  match (CppVar ident) = return ident
  match (CppInstance [] (cls, _) _ params) = return $ cls ++ '<' : intercalate "," (snd <$> params) ++ ">"
  match (CppInstance mn (cls, _) _ params) = return $ mn ++ "::" ++ cls ++ '<' : intercalate "," (snd <$> params) ++ ">"
  match (CppScope ident) = return ident
--   match (CppApp v [CppVar name]) | "__dict_" `isPrefixOf` name = return (prettyPrintCpp1 v) -- TODO: ugly
  match (CppApp v [CppNoOp]) = return (prettyPrintCpp1 v)
  match (CppVariableIntroduction (ident, typ) value) = fmap concat $ sequence
    [ return ((if null typ then "auto" else typ) ++ " ")
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintCpp') value
    , return ";"
    ]
  match (CppAssignment target value) = fmap concat $ sequence
    [ prettyPrintCpp' target
    , return " = "
    , prettyPrintCpp' value
    , return ";"
    ]
  match (CppWhile cond sts) = fmap concat $ sequence
    [ return "while ("
    , prettyPrintCpp' cond
    , return ") "
    , prettyPrintCpp' sts
    ]
  match (CppFor ident start end sts) = fmap concat $ sequence
    [ return $ "for (auto " ++ ident ++ " = "
    , prettyPrintCpp' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintCpp' end
    , return $ "; " ++ ident ++ "++) "
    , prettyPrintCpp' sts
    ]
  match (CppForIn ident obj sts) = fmap concat $ sequence
    [ return $ "for (auto " ++ ident ++ " : "
    , prettyPrintCpp' obj
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
  match (CppReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintCpp' value
    , return ";"
    ]
  match (CppThrow value) = fmap concat $ sequence
    [ return "throw "
    , prettyPrintCpp' value
    , return ";"
    ]
  match (CppBreak lbl) = return $ "goto " ++ lbl ++ ";"
  match (CppContinue _) = return $ "continue;"
  match (CppLabel lbl cpp) = fmap concat $ sequence
    [ return $ lbl ++ ": "
    , prettyPrintCpp' cpp
    ]
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
string s = '"' : concatMap encodeChar s ++ "\""
  where
  encodeChar :: Char -> String
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c = [c]

conditional :: Pattern PrinterState Cpp ((Cpp, Cpp), Cpp)
conditional = mkPattern match
  where
  match (CppConditional cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern PrinterState Cpp (String, Cpp)
accessor = mkPattern match
  where
  match (CppAccessor prop val@CppVar{}) = Just (prop, val)
  match _ = Nothing

scope :: Pattern PrinterState Cpp (String, Cpp)
scope = mkPattern match
  where
  match (CppAccessor prop val@CppScope{}) = Just (prop, val)
  match (CppApp val [inst@CppInstance{}]) =
    let val' = case val of
                 (CppAccessor v (CppScope _)) -> v
                 _ -> prettyPrintCpp1 val
        inst' = CppScope (prettyPrintCpp1 inst)
    in Just (if '<' `elem` val' then "template " ++ val' else val', inst')
  match _ = Nothing

indexer :: Pattern PrinterState Cpp (String, Cpp)
indexer = mkPattern' match
  where
  match (CppIndexer index val) = (,) <$> prettyPrintCpp' index <*> pure val
  match _ = mzero

fun :: Pattern PrinterState Cpp ((String, String, [(String, String)], String, [String]), Cpp)
fun = mkPattern' match
  where
  match (CppFunction name tmps args rty qs ret) = do
    indentString <- currentIndent
    let templ = if null tmps then [] else templDecl' (Left tmps) ++ "\n" ++ indentString
    return ((templ, name, args, rty, runQualifier <$> qs), ret)
  match _ = mzero

lam :: Pattern PrinterState Cpp (([(String, String)], String), Cpp)
lam = mkPattern match
  where
  match (CppLambda args rty ret) = Just ((args, rty), ret)
  match _ = Nothing

app :: Pattern PrinterState Cpp (String, Cpp)
app = mkPattern' match
  where
  match (CppApp _ [CppInstance{}]) = mzero
--   match (CppApp _ [CppVar name]) | "__dict_" `isPrefixOf` name = mzero -- TODO: ugly
  match (CppApp _ [CppNoOp]) = mzero
  match (CppApp val args) = do
    cpps <- mapM prettyPrintCpp' args
    return (intercalate ", " cpps, val)
  match _ = mzero

partapp :: Pattern PrinterState Cpp ((String, Int), Cpp)
partapp = mkPattern' match
  where
  match (CppPartialApp (CppAccessor val' _) (inst@CppInstance{} : args) n) = do
    inst' <- prettyPrintCpp' inst
    cpps <- mapM prettyPrintCpp' (CppAccessor val' (CppScope inst') : args)
    return ((intercalate ", " cpps, n), CppNoOp)
  match (CppPartialApp val (inst@CppInstance{} : args) n) = do
    val' <- prettyPrintCpp' val
    inst' <- prettyPrintCpp' inst
    cpps <- mapM prettyPrintCpp' (CppAccessor val' (CppScope inst') : args)
    return ((intercalate ", " cpps, n), CppNoOp)
  match (CppPartialApp val args n) = do
    cpps <- mapM prettyPrintCpp' (val : args)
    return ((intercalate ", " cpps, n), CppNoOp)
  match _ = mzero

typeOf :: Pattern PrinterState Cpp ((), Cpp)
typeOf = mkPattern match
  where
  match (CppTypeOf val) = Just ((), val)
  match _ = Nothing

instanceOf :: Pattern PrinterState Cpp (Cpp, Cpp)
instanceOf = mkPattern match
  where
  match (CppInstanceOf val ty) = Just (val, ty)
  match _ = Nothing

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
binary op str = AssocL match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  match :: Pattern PrinterState Cpp (Cpp, Cpp)
  match = mkPattern match'
    where
    match' (CppBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [Cpp] -> StateT PrinterState Maybe String
prettyStatements sts = do
  cpps <- forM (filter (/=CppNoOp) sts) prettyPrintCpp'
  indentString <- currentIndent
  return $ intercalate "\n" $ map (indentString ++) cpps

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
    OperatorTable [ [ Wrap accessor $ \prop val -> "(*" ++ val ++ ")." ++ prop ]
                  , [ Wrap scope $ \prop val -> val ++ "::" ++ prop ]
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args val -> val ++ "(" ++ args ++ ")" ]
                  , [ Wrap partapp $ \(args, n) _ -> "bind" ++ show n ++ '(' : args ++ ")" ]
                  , [ unary CppNew "new " ]
                  , [ Wrap fun $ \(templ, name, args, rty, quals) ret -> []
                        ++ templ
                        ++ concatMap (++ " ") quals
                        ++ "auto "
                        ++ name
                        ++ let args' = argstr <$> args in
                           "(" ++ intercalate ", " args' ++ ")"
                        ++ " -> "
                        ++ rty
                        ++ if null ret then ";" else ' ' : ret ]
                  , [ Wrap lam $ \(args, rty) ret -> "[=] "
                        ++ let args' = argstr <$> args in
                           "(" ++ intercalate ", " args' ++ ")"
                        ++ (if null rty then [] else " -> " ++ rty)
                        ++ " "
                        ++ ret ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
                  , [ unary     CppNot                "!"
                    , unary     CppBitwiseNot         "~"
                    , unary     CppPositive           "+"
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
                    , binary    LessThanOrEqual      "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqual   ">="
                    , AssocR instanceOf $ \v1 v2 -> v1 ++ " instanceof " ++ v2 ]
                  , [ binary    Equal                "=="
                    , binary    NotEqual             "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintCpp1 th ++ " : " ++ prettyPrintCpp1 el ]
                    ]

dotsTo :: Char -> String -> String
dotsTo chr = map (\c -> if c == '.' then chr else c)

classstr :: (String, [String]) -> String
classstr (name, []) = name
classstr (name, params) = name ++ '<' : intercalate ", " params ++ ">"

argstr :: (String, String) -> String
argstr ([], typ) = typ
argstr (name, []) = "auto " ++ name
argstr (name, typ) = typ ++ ' ' : name

templDecl :: [(String, Int)] -> String
templDecl ps = "template <" ++ intercalate ", " (go <$> ps) ++ ">"
  where
  go :: (String, Int) -> String
  go (name, 0) = "typename " ++ name
  go (name, n) = templDecl (flip (,) 0 . (('_' : name) ++) . show <$> [1..n]) ++ " class " ++ name

templDecl' :: Either [(String, Int)] [String] -> String
templDecl' (Left []) = []
templDecl' (Left params) = templDecl params
templDecl' (Right []) = []
templDecl' (Right _) = templDecl []
