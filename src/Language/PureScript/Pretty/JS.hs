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

import Control.Applicative
import Control.Arrow ((<+>))
import Control.Monad.State
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Pretty.Common

import Numeric

import Language.PureScript.CodeGen.Go
import Debug.Trace

literals :: Pattern PrinterState JS String
literals = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ either show show n
  match (JSStringLiteral s) = return $ string s
  match (JSBooleanLiteral True) = return "true"
  match (JSBooleanLiteral False) = return "false"
  match (JSArrayLiteral []) = fmap concat $ sequence
    [ return $ anyList ++ "{}"
    , return ""
    ]
  match (JSArrayLiteral xs) = fmap concat $ sequence
    [ return "[ "
    , fmap (intercalate ", ") $ forM xs prettyPrintJS'
    , return " ]"
    ]
  match (JSObjectLiteral []) = return "nil"
  match (JSObjectLiteral ps) = fmap concat $ sequence
    [ return anyMap
    , return "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key ++ ": ") ++) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ concatMap (\s -> s ++ ",\n") $ map (indentString ++) jss
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    objectPropertyToString :: String -> String
    objectPropertyToString = show
  match (JSBlock sts) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSVar ident) = return ident
  match (JSVariableIntroduction ident value) =
    let atModLevel = '.' `elem` ident in
    fmap concat $ sequence $
    if ident == "Main.main" then [return $ funcDecl ++ "main() {",
                                  return "\n",
                                  maybe (return "") (fmap ("  " ++) . prettyPrintJS') value,
                                  return "\n",
                                  return $ "}"]
    else case value of
      (Just (JSFunction Nothing [arg] (JSBlock ret))) ->
          if atModLevel then let arg' = argOnly arg
                                 typ' = typeOnly arg in
                             [return funcDecl,
                              return (modulePrefix ++ unqual ident),
                              return . parens $ arg' ++ withSpace anyType,
                              return " ",
                              return anyType, -- TODO: look for JSReturn and set accordingly?
                              return " ",
                              prettyPrintJS' . JSBlock $
                              if typ' == anyType
                                then ret
                                else [JSBlock (JSVariableIntroduction arg' (Just $ withCast typ' (JSVar arg')) : ret)]
                              ]
                        else [return "var ",
                              return ident,
                              return $ withSpace funcDecl,
                              return (parens anyType),
                              return " ",
                              return anyType, -- TODO: look for JSReturn and set accordingly?
                              return "\n",
                              currentIndent,
                              withIndent $ return ident,
                              maybe (return "") (fmap (" = " ++) . prettyPrintJS') value]

      (Just (JSData' prefix fields)) ->
           [prettyPrintJS' (JSData' (prefix ++ unqual ident) fields)]

      (Just (JSInit a b)) ->
           [return "var ",
            return (modulePrefix ++ unqual ident),
            return " ",
            prettyPrintJS' a,
            return "\n",
            return $ funcDecl ++ "init() { ",
            return "\n",
            withIndent $ do
              indentString <- currentIndent
              return (indentString ++ modulePrefix ++ unqual ident),
            return " = ",
            prettyPrintJS' a,
            return "{\n",
            withIndent $ withIndent $ do
              jss <- forM b prettyPrintJS'
              indentString <- currentIndent
              return $ concatMap (\s -> s ++ ",\n") $ map (indentString ++) jss,
            withIndent $ do
              indentString <- currentIndent
              return indentString,
            return "}",
            return "\n",
            return "}",
            return "\n"]

      (Just (JSObjectLiteral [])) ->
           [return "var ",
            return $ if atModLevel then modulePrefix else "",
            return $ unqual ident,
            return " ",
            return "struct{}"]

      (Just (JSBlock b)) ->
           [return $ prettyPrintJS b]

      _ -> [return "var ",
            return $ if atModLevel then modulePrefix else "",
            return $ unqual ident,
            maybe (return "") (fmap (" = " ++) . prettyPrintJS') value]

  match (JSAssignment target value) = fmap concat $ sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    ]
  match (JSWhile cond sts) = fmap concat $ sequence
    [ return "for "
    , do c <- prettyPrintJS' cond
         return (if c == "true" then "" else c)
    , return " "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = fmap concat $ sequence
    [ return $ "for " ++ ident ++ " := "
    , prettyPrintJS' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintJS' end
    , return $ "; " ++ ident ++ "++ "
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = fmap concat $ sequence
    [ return $ "for (" ++ ident ++ " in "
    , prettyPrintJS' obj
    , return " "
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = fmap concat $ sequence
    [ return "if "
    , prettyPrintJS' $ condition cond
    , return " "
    , prettyPrintJS' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintJS') elses
    ]
  match (JSReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintJS' value
    ]
  match (JSThrow value) = fmap concat $ sequence
    [ return "panic("
    , prettyPrintJS' value
    , return ")"
    ]
  match (JSBreak lbl) = return $ "break " ++ lbl
  match (JSContinue lbl) = return $ "goto " ++ lbl
  match (JSLabel lbl js) = fmap concat $ sequence
    [ return $ lbl ++ ": "
    , prettyPrintJS' js
    ]
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

accessor :: Pattern PrinterState JS (String, JS)
accessor = mkPattern match
  where
  match (JSAccessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState JS (String, JS)
indexer = mkPattern' match
  where
  match (JSIndexer index val) = (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState JS ((Maybe String, [String]), JS)
lam = mkPattern match
  where
  match (JSFunction name args ret) = Just ((name, args), ret)
  match _ = Nothing

dat' :: Pattern PrinterState JS (String, JS)
dat' = mkPattern match
  where
  match (JSData' name fields) = Just (name, fields)
  match _ = Nothing

app :: Pattern PrinterState JS (String, JS)
app = mkPattern' match
  where
  match (JSApp val args) = do
    jss <- mapM prettyPrintJS' (val:args)
    return (intercalate ", " jss, val)
  match _ = mzero

app' :: Pattern PrinterState JS (String, JS)
app' = mkPattern' match
  where
  match (JSApp' val args) = do
    jss <- mapM prettyPrintJS' args
    return (intercalate ", " jss, val)
  match (JSApp val@(JSFunction _ _ _) args) = do
    jss <- mapM prettyPrintJS' args
    return $ (intercalate ", " jss, val)
  match _ = mzero

init' :: Pattern PrinterState JS (String, JS)
init' = mkPattern' match
  where
  match (JSInit val args) =
    case args of
      [] -> return ([], val)
      _ -> do
           fields <- withIndent $ do
                  jss <- forM args prettyPrintJS'
                  indentString <- currentIndent
                  return $ concatMap (\s -> s ++ ",\n") $ map (indentString ++) jss
           indentString <- currentIndent
           return ('\n' : fields ++ indentString, val)
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

unary :: UnaryOperator -> String -> Operator PrinterState JS String
unary op str = Wrap match (++)
  where
  match :: Pattern PrinterState JS (String, JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (str, val)
    match' _ = Nothing

binary :: BinaryOperator -> String -> Operator PrinterState JS String
binary op str = AssocL match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  match :: Pattern PrinterState JS (JS, JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [JS] -> StateT PrinterState Maybe String
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate "\n" $ map (indentString ++) jss

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
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args _ -> appFn ++ parens args ]
                  , [ Wrap app' $ \args val -> val ++ "(" ++ args ++ ")" ]
                  , [ Wrap init' $ \args val -> val ++ "{" ++ args ++ "}" ]
                  , [ unary JSNew "new " ]
                  , [ Wrap lam $ \(name, args) ret -> funcDecl
                        ++ fromMaybe "" name
                        ++ parens (intercalate "," (map addTypeIfNeeded args))
                        ++ " "
                        ++ anyType
                        ++ " "
                        ++ ret ]
                  , [ Wrap dat' $ \name fields -> "\n"
                        ++ "type "
                        ++ name
                        ++ " struct "
                        ++ fields ]
                  , [ binary    LessThan             "<" ]
                  , [ binary    LessThanOrEqualTo    "<=" ]
                  , [ binary    GreaterThan          ">" ]
                  , [ binary    GreaterThanOrEqualTo ">=" ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
                  , [ AssocR instanceOf $ \v1 v2 -> typeOfExpr v1 ++ " == " ++ typeOfType v2 ]

                  , [ unary     Not                  "!" ]
                  , [ unary     BitwiseNot           "~" ]
                  , [ unary     Negate               "-" ]
                  , [ unary     Positive             "+" ]
                  , [ binary    Multiply             "*" ]
                  , [ binary    Divide               "/" ]
                  , [ binary    Modulus              "%" ]
                  , [ binary    Add                  "+" ]
                  , [ binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<" ]
                  , [ binary    ShiftRight           ">>" ]
                  , [ binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    EqualTo              "==" ]
                  , [ binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el ]
                    ]
