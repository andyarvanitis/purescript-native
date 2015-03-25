-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for the Javascript AST
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.JS (
    prettyPrintJS
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Char

import Control.Applicative
import Control.Arrow ((<+>))
import Control.Monad.State
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Pretty.Common
import Language.PureScript.Comments

import Numeric

literals :: Pattern PrinterState JS String
literals = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ either show show n
  match (JSStringLiteral s) = return $ string s
  match (JSBooleanLiteral True) = return "true"
  match (JSBooleanLiteral False) = return "false"
  match (JSArrayLiteral xs) = fmap concat $ sequence
    [ return "[ "
    , fmap (intercalate ", ") $ forM xs prettyPrintJS'
    , return " ]"
    ]
  match (JSObjectLiteral []) = return "{}"
  match (JSObjectLiteral ps) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key ++ " => ") ++) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) jss
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    objectPropertyToString :: String -> String
    objectPropertyToString s | identNeedsEscaping s = show s
                             | otherwise = s
  match (JSBlock sts) = fmap concat $ sequence
    [ return "\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "end"
    ]
  match (JSVar ident) = return ident
  match (JSVariableIntroduction ident@(c:_) (Just (JSFunction _ args (JSBlock sts))))
    | isUpper c = fmap concat $ sequence
    [ return ""
    , return $ "class " ++ ident ++ " < Prelude::TypeClass"
    , return "\n"
    , withIndent $ do
        indentString <- currentIndent
        withIndent $ do
          indentString' <- currentIndent
          sts' <- prettyStatements sts
          return $ indentString ++ "def initialize(" ++ (intercalate ", " args) ++ ")"
                   ++ "\n"
                   ++ indentString' ++ "super()\n"
                   ++ sts'
                   ++ "\n"
                   ++ indentString ++ "end\n"
                   ++ concatMap (\v -> indentString ++ "attr_reader :" ++ v ++ "\n") args
    , currentIndent
    , return "end"
    ]
  match (JSVariableIntroduction ident@(c:_)
        (Just (JSApp (JSFunction Nothing [] (JSBlock ((JSFunction _ fields _):_))) _)))
    | isUpper c = fmap concat $ sequence
    [ return $ "class " ++ ident
    , return "\n"
    , if null fields then
        return []
      else
        withIndent $ do
          indentString <- currentIndent
          withIndent $ do
            indentString' <- currentIndent
            return $ indentString ++ "def initialize(" ++ (intercalate ", " fields) ++ ")"
                     ++ concatMap (\v -> '\n' : indentString' ++ '@' : v ++ " = " ++ v) fields
                     ++ "\n"
                     ++ indentString ++ "end\n"
    , currentIndent
    , return "end"
    ]

  match (JSFunction _ [] (JSBlock [JSReturn st])) | isSimpleExpr st
    = fmap concat $ sequence
    [ return "->{ "
    , prettyPrintJS' st
    , return " }"
    ]
  match (JSFunction _ args (JSBlock [JSReturn st])) | isSimpleExpr st
    = fmap concat $ sequence
    [ return "lambda { "
    , return $ if null args then [] else '|' : (intercalate ", " args) ++ "| "
    , prettyPrintJS' st
    , return " }"
    ]
  match (JSFunction _ args (JSBlock sts)) = fmap concat $ sequence
    [ return "lambda do "
    , return $ if null args then [] else '|' : (intercalate ", " args) ++ "|"
    , return "\n"
    , withIndent $ do
      indentString' <- currentIndent
      sts' <- prettyStatements sts
      return $ sts' ++ "\n"
    , currentIndent
    , return "end"
    ]
  match (JSUnary JSNew (JSApp val [])) = fmap concat $ sequence
    [ prettyPrintJS' val
    , return ".new()"
    ]
  match (JSUnary JSNew (JSApp val args)) = fmap concat $ sequence
    [ prettyPrintJS' val
    , return ".new("
    , if all isSimpleExpr args then do
        args' <- mapM prettyPrintJS' args
        return $ intercalate ", " args' ++ ")"
      else
        withIndent $ do
          indentString <- currentIndent
          ss <- mapM (prettyStatements . (\a -> [a])) args
          return $ '\n' : intercalate ",\n" ss ++ '\n' : indentString ++ ")"
    ]
  match (JSVariableIntroduction ident value) = fmap concat $ sequence
    [ return ""
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintJS') value
    ]
  match (JSAssignment target value) = fmap concat $ sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    ]
  match (JSWhile cond sts) = fmap concat $ sequence
    [ return "while "
    , prettyPrintJS' cond
    , return " "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = fmap concat $ sequence
    [ return $ "for " ++ ident ++ " in "
    , prettyPrintJS' start
    , return $ " .. "
    , prettyPrintJS' end
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = fmap concat $ sequence
    [ return $ "for " ++ ident ++ " in "
    , prettyPrintJS' obj
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = fmap concat $ sequence
    [ return "if "
    , prettyPrintJS' cond
    , return " "
    , prettyPrintJS' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintJS') elses
    ]
  match (JSReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintJS' value
    ]
  match (JSThrow value) = fmap concat $ sequence
    [ return "raise "
    , prettyPrintJS' value
    ]
  match (JSBreak lbl) = return $ "throw :" ++ lbl
  match (JSContinue _) = return $ "redo " -- ++ lbl
  match (JSLabel lbl js) = fmap concat $ sequence
    [ return $ "catch :" ++ lbl ++ " do"
    , prettyPrintJS' js
    , return "end"
    ]
  match (JSComment com js) = fmap concat $ sequence $
    [ return "\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return "#\n"
    , currentIndent
    , prettyPrintJS' js
    ]
    where
    commentLines :: Comment -> [String]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = lines s

    asLine :: String -> StateT PrinterState Maybe String
    asLine s = do
      i <- currentIndent
      return $ i ++ "# " ++ removeComments s ++ "\n"

    removeComments :: String -> String
    removeComments ('#' : '/' : s) = removeComments s
    removeComments (c : s) = c : removeComments s

    removeComments [] = []
  match (JSRaw js) = return js
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

conditional :: Pattern PrinterState JS ((JS, JS), JS)
conditional = mkPattern match
  where
  match (JSConditional cond th el) = Just ((th, el), cond)
  match _ = Nothing

instvar :: Pattern PrinterState JS (String, JS)
instvar = mkPattern match
  where
  match (JSAccessor prop val@(JSVar "this")) = Just (prop, val)
  match _ = Nothing

accessor :: Pattern PrinterState JS (String, JS)
accessor = mkPattern match
  where
  match (JSAccessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState JS (String, JS)
indexer = mkPattern' match
  where
  match (JSIndexer index val) = (,) <$> prettyPrintJS' index <*> pure val'
    where
    val' | (JSVar "this") <- val = JSVar "self"
         | otherwise = val
  match _ = mzero

-- lam :: Pattern PrinterState JS ((Maybe String, [String]), JS)
-- lam = mkPattern match
--   where
--   match (JSFunction name args ret) = Just ((name, args), ret)
--   match _ = Nothing

app :: Pattern PrinterState JS (String, JS)
app = mkPattern' match
  where
  match (JSApp val args) = do
    jss <- mapM prettyPrintJS' args
    return (intercalate ", " jss, val)
  match _ = mzero

typeOf :: Pattern PrinterState JS ((), JS)
typeOf = mkPattern match
  where
  match (JSTypeOf val) = Just ((), val)
  match _ = Nothing

instanceOf :: Pattern PrinterState JS (JS, JS)
instanceOf = mkPattern match
  where
  match (JSInstanceOf val ty) = Just (val, ty)
  match _ = Nothing

unary' :: UnaryOperator -> (JS -> String) -> Operator PrinterState JS String
unary' op mkStr = Wrap match (++)
  where
  match :: Pattern PrinterState JS (String, JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (mkStr val, val)
    match' _ = Nothing

unary :: UnaryOperator -> String -> Operator PrinterState JS String
unary op str = unary' op (const str)

negateOperator :: Operator PrinterState JS String
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (JSUnary Negate _) = True
  isNegate _ = False

binary :: BinaryOperator -> String -> Operator PrinterState JS String
binary op str = AssocL match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  match :: Pattern PrinterState JS (JS, JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

isSimpleExpr :: JS -> Bool
isSimpleExpr JSNumericLiteral{} = True
isSimpleExpr JSStringLiteral{} = True
isSimpleExpr JSBooleanLiteral{} = True
isSimpleExpr JSVar{} = True
isSimpleExpr JSIndexer{} = True
isSimpleExpr JSAccessor{} = True
isSimpleExpr (JSBlock []) = True
isSimpleExpr _ = False

prettyStatements :: [JS] -> StateT PrinterState Maybe String
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate "\n" $ map ((++ "") . (indentString ++)) jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
prettyPrintJS1 :: JS -> String
prettyPrintJS1 = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintJS'

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintJS :: [JS] -> String
prettyPrintJS = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintJS' :: JS -> StateT PrinterState Maybe String
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState JS String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState JS String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap instvar $ \prop _ -> '@' : prop ]
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args val -> val ++ "[" ++ args ++ "]" ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
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
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> v1 ++ ".instance_of " ++ v2 ]
                  , [ binary    EqualTo              "=="
                    , binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "and" ]
                  , [ binary    Or                   "or" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el ]
                    ]
