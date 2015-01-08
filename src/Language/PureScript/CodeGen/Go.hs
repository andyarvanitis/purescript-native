module Language.PureScript.CodeGen.Go where

import Data.List

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Pretty.Common

unqual :: String -> String
unqual s = let indices = elemIndices '.' s in
	         if null indices then s else drop (last indices + 1) s

dotsTo :: Char -> String -> String
dotsTo chr = map (\c -> if c == '.' then chr else c)

anyType :: String
anyType  = "interface{}"

funcDecl :: String
funcDecl = "func "

anyFunc :: String
anyFunc  = funcDecl ++ parens (anyType) ++ withSpace anyType

noArgFunc :: String
noArgFunc  = funcDecl ++ parens [] ++ withSpace anyType

appFn :: String
appFn = "ApplyFn"

anyList :: String
anyList = "[]" ++ anyType

anyMap :: String
anyMap = "map [string] " ++ anyType

getSuper :: String
getSuper = funcDecl ++ parens "" ++ withSpace anyType

capitalize :: String -> String
--capitalize (x:xs) | isLower x = (toUpper x : xs)
capitalize s = s

appFnDef :: [JS]
appFnDef = map JSRaw [
              funcDecl ++ appFn ++ parens ("f " ++ anyType ++ ", args ..." ++ anyType) ++ " " ++ anyType ++ " {"
            , "  count := len(args)"
            , "  if count == 0 {"
            , "    f = f." ++ parens (noArgFunc) ++ "()"
            , "  } else {"
            , "    for i := 0; i < count; i++ {"
            , "      f = f." ++ parens (anyFunc) ++ "(args[i])"
            , "    }"
            , "  }"
            , "  return f"
            , "}"
            ]

withSpace :: String -> String
withSpace [] = []
withSpace s = (' ' : s)

listLen :: JS -> JS
listLen l = JSApp' (JSVar "len") [JSAccessor (parens anyList) l]

typeOfExpr :: String -> String
typeOfExpr s = "reflect.TypeOf" ++ parens s

typeOfType :: String -> String
typeOfType s = typeOfExpr (parens ('*': s) ++ parens "nil") ++ ".Elem()"

addTypeIfNeeded :: String -> String
addTypeIfNeeded [] = []
addTypeIfNeeded s = s ++ if length (words s) > 1 then [] else withSpace anyType

argOnly :: String -> String
argOnly = head . words

typeOnly :: String -> String
typeOnly s
  | length (words s) > 1 = intercalate " " . tail $ words s
  | otherwise = anyType

withCast :: String -> JS -> JS
withCast = JSAccessor . parens

condition :: JS -> JS
condition cond = case cond of
                   (JSVar _)          -> JSBinary EqualTo cond (JSBooleanLiteral True)
                   (JSUnary Not c)    -> JSBinary EqualTo c (JSBooleanLiteral False)
                   (JSBinary And a b) -> JSBinary And (condition a) (condition b)
                   (JSBinary Or a b)  -> JSBinary Or (condition a) (condition b)
                   _                  -> cond

modulePrefix :: String
modulePrefix = "M_"

typeclassPrefix :: String
typeclassPrefix = "T_"

ctorSuffix :: String
ctorSuffix = "_Ctor"
