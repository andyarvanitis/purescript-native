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
import Data.Char (isDigit, toUpper)

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
import Debug.Trace

literals :: Pattern PrinterState JS String
literals = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ either show show n
  match (JSStringLiteral s) = return $ string s
  match (JSBooleanLiteral True) = return "true"
  match (JSBooleanLiteral False) = return "false"
  match (JSArrayLiteral xs) = fmap concat $ sequence
    [ return "{"
    , fmap (intercalate ", ") $ forM xs prettyPrintJS'
    , return "}"
    ]
  match (JSObjectLiteral []) = return "nullptr"
  match (JSObjectLiteral ps) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key ++ ": ") ++) . prettyPrintJS' $ value
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
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSNamespace name sts) =
    if any notNoOp sts then fmap concat $ sequence $
        [ return $ "namespace " ++ name ++ " {\n"
        , withIndent $ prettyStatements sts
        , return "\n"
        , currentIndent
        , return "}"
        ]
    else
      return []
  match (JSSequence s sts) =
    if any notNoOp sts then fmap concat $ sequence $
        (if (not . null) s then ((return $ "// " ++ s) : ) else id)
        [ return "\n"
        , prettyStatements $ sts ++ if (not . null) s then [JSRaw "//"] else []
        ]
    else return []
  match (JSVar ident) = return $ takeWhile (/='@') ident
  match (JSVariableIntroduction name (Just (JSFunction (Just fname) args sts))) = fmap concat $ sequence $
    let ns = words name in
    [ if null (dropWhile (/= '|') fname) then
        return []
      else do
        indentString <- currentIndent
        return $ (if head ns == "extern" then
                    "extern template"
                  else
                    "template <" ++ (takeWhile (/= '|') fname) ++ ">")
               ++ if notNoOp sts then '\n':indentString else " "
    , do
      indentString <- currentIndent
      return $ case head ns of
                 "inline" -> "inline auto " ++ last ns
                 "static" -> name ++ " =\n" ++ indentString ++ "[=]"
                 _ -> "auto " ++ last ns
    , return "("
    , return $ intercalate ", " $ cleanParams args
    , return ")"
    , return $ returnType fname
    , if notNoOp sts then do
        s <- prettyPrintJS' sts
        return $ ' ' : s ++ " " ++ if head ns == "static" then ";" else []
      else
        return ";"
    ]
  -- TODO: some of this should be moved out of here -- maybe apply optim to data too?
  --
  match (JSVariableIntroduction name (Just (JSData ctor typename [typestr] JSNoOp))) = fmap concat $ sequence
    [ do indentString <- currentIndent
         return $ templateDecl indentString typename
    , do indentString <- currentIndent
         return $ "using " ++ takeWhile (/='@') typename ++ " = " ++ typestr ++ ";\n" ++ indentString
    , do indentString <- currentIndent
         return $ templateDecl indentString typename
    , do indentString <- currentIndent
         return $ "using " ++ ctor ++ " = " ++ typestr ++ ";\n" ++ indentString
    , do indentString <- currentIndent
         return $ templateDecl indentString typename
    , return $ "struct _" ++ ctor ++ "_ {"
    , return "\n"
    , withIndent $ do
         indentString <- currentIndent
         f <- match $ JSVariableIntroduction "ctor" $ Just $
                JSFunction (Just $ typestr ++ " _") [typestr ++ ' ' : "value"] $
                  JSBlock [JSReturn (JSApp (JSVar typestr) [JSVar "value"])]
         return $ indentString ++ "static " ++ f
    , currentIndent
    , return "\n"
    , do indentString <- currentIndent
         return $ indentString ++ "};"
    ]
  match (JSVariableIntroduction name (Just (JSData ctor typename fs fn))) =
    let fields = cleanParams fs in fmap concat $ sequence
    [ do indentString <- currentIndent
         return $ templateDecl indentString typename
    , return "struct "
    , return ctor
    , return " : public "
    , return $ dataType typename ++ " {"
    , return "\n"
    , withIndent $ do
        indentString <- currentIndent
        return $ concatMap ((++ ";\n") . (indentString ++)) fields
    , withIndent $ do
        indentString <- currentIndent
        let vals = last . words <$> fields
        return $ indentString ++ ctor ++ parens (intercalate ", " fields)
              ++ (if null fields then [] else " : ")
              ++ intercalate ", " (map (\v -> v ++ parens v) vals)
              ++ " {}"
    , return "\n"
    , currentIndent
    , currentIndent
    , withIndent $ do
        f <- match fn
        return $ "static " ++ f
    , return "\n"
    , do
        indentString <- currentIndent
        return $ indentString ++ "};"
    ]
  match (JSVariableIntroduction ident value) = fmap concat $ sequence
    [ return "auto "
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintJS') value
    , return ";"
    ]
  match (JSAssignment target value) = fmap concat $ sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    , return ";"
    ]
  match (JSWhile cond sts) = fmap concat $ sequence
    [ return "while ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = fmap concat $ sequence
    [ return $ "for (var " ++ ident ++ " = "
    , prettyPrintJS' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintJS' end
    , return $ "; " ++ ident ++ "++) "
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = fmap concat $ sequence
    [ return $ "for (var " ++ ident ++ " in "
    , prettyPrintJS' obj
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = fmap concat $ sequence
    [ return "if ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintJS') elses
    ]
  match (JSReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintJS' value
    , return ";"
    ]
  match (JSThrow value) = fmap concat $ sequence
    [ return "throw "
    , prettyPrintJS' value
    , return ";"
    ]
  match (JSBreak lbl) = return []
  match (JSContinue lbl) = return []
  match (JSLabel lbl js) = fmap concat $ sequence
    [ prettyPrintJS' js
    ]
  match (JSComment _ js) = match js
  -- match (JSComment com js) = fmap concat $ sequence $
  --   [ return "\n"
  --   , currentIndent
  --   , return "/**\n"
  --   ] ++
  --   map asLine (concatMap commentLines com) ++
  --   [ currentIndent
  --   , return " */\n"
  --   , currentIndent
  --   , prettyPrintJS' js
  --   ]
  --   where
  --   commentLines :: Comment -> [String]
  --   commentLines (LineComment s) = [s]
  --   commentLines (BlockComment s) = lines s
  --
  --   asLine :: String -> StateT PrinterState Maybe String
  --   asLine s = do
  --     i <- currentIndent
  --     return $ i ++ " * " ++ removeComments s ++ "\n"
  --
  --   removeComments :: String -> String
  --   removeComments ('*' : '/' : s) = removeComments s
  --   removeComments (c : s) = c : removeComments s
  --
  --   removeComments [] = []
  match JSEndOfHeader = return []
  match (JSRaw js) = return js
  match JSNoOp = return []
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

cast :: Pattern PrinterState JS (JS, JS)
cast = mkPattern match
  where
  match (JSCast ty val) = Just (ty, val)
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
binary op str = AssocL match (\v1 v2 -> cleanParam v1 ++ " " ++ str ++ " " ++ cleanParam v2)
  where
  match :: Pattern PrinterState JS (JS, JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [JS] -> StateT PrinterState Maybe String
prettyStatements sts = do
  jss <- forM (filter notNoOp sts) prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate "\n" $ map (indentString ++) (filter (not . null) jss)

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
                  , [ Wrap app $ \args val -> filter (/='#') val ++ "(" ++ args ++ ")" ]
                  , [ unary JSNew "new " ]
                  , [ Wrap lam $ \(name, args) ret -> let args' = cleanParams args in
                           "[=]"
                        ++ "(" ++ intercalate ", " (map (\a -> if length (words a) < 2 then ("auto" ++ a) else a) args') ++ ")"
                        ++ maybe "" returnType name
                        ++ " "
                        ++ ret
                    ]
                  , [ AssocR cast $ \typ val -> "cast<" ++ typ ++ ">" ++ parens val ]
                  , [ binary    LessThan             "<" ]
                  , [ binary    LessThanOrEqualTo    "<=" ]
                  , [ binary    GreaterThan          ">" ]
                  , [ binary    GreaterThanOrEqualTo ">=" ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
                  , [ AssocR instanceOf $ \v1 v2 -> "instanceof<" ++ v2 ++ ">" ++ parens v1 ]
                  , [ unary     Not                  "!" ]
                  , [ unary     BitwiseNot           "~" ]
                  , [ negateOperator ]
                  , [ unary     Positive             "+" ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<" ]
                  , [ binary    ShiftRight           ">>" ]
                  , [ binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    EqualTo              "==" ]
                  , [ binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el ]
                    ]

notNoOp :: JS -> Bool
notNoOp JSNoOp = False
notNoOp (JSSequence _ []) = False
notNoOp (JSNamespace _ []) = False
notNoOp (JSComment _ js) = notNoOp js
notNoOp (JSVariableIntroduction _ (Just js)) = notNoOp js
notNoOp _ = True

templateDecl :: String -> String -> String
templateDecl sp s
  | t@('[':_:_:_) <- drop 1 $ dropWhile (/='@') s =
      "template " ++ '<' : intercalate ", " (tname <$> read t) ++ ">\n" ++ sp
  | otherwise = []
  where
    tname (s:ss) = "typename " ++ (toUpper s : ss)

dataType :: String -> String
dataType s = takeWhile (/='@') s ++ case ty of
                                      [] -> []
                                      "[]" -> []
                                      _  -> '<' : intercalate "," (read ty) ++ ">"
  where ty = drop 1 $ dropWhile (/='@') s

returnType :: String -> String
returnType fn
  | ws <- words $ drop ((fromMaybe (-1) $ elemIndex '|' fn) + 1) fn,
    length ws > 1,
    name@(_:_) <- stripped ws = " -> " ++ name
  | otherwise = []
  where
    stripped ws = cleanParam . intercalate " " . init . filter (/="inline") $ ws

cleanParam :: String -> String
cleanParam s
  | hs@(_:_) <- dropWhile (/='#') s = takeWhile (/='#') s ++ (cleanParam $ unhash hs)
 where
   unhash :: String -> String
   unhash ('#':c:cs)
     | str@(_:_) <- dropWhile isDigit (c:cs) = str
     | otherwise = c:cs
   unhash cs = cs
cleanParam s = s

cleanParams :: [String] -> [String]
cleanParams = map cleanParam
