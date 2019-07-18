-- | Common code generation utility functions
module CodeGen.IL.Common where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)
import Data.Char
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names

import qualified Language.PureScript.Constants as C

moduleNameToIL :: ModuleName -> Text
moduleNameToIL (ModuleName pns) =
  let name = T.intercalate "_" (runProperName `map` pns)
  in moduleNameToIL' name

moduleNameToIL' :: Text -> Text
moduleNameToIL' name =
  if moduleIsReserved name then (name <> "_") else name

-- | Convert an 'Ident' into a valid IL identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved IL identifiers are wrapped with '_'.
--
identToIL :: Ident -> Text
identToIL UnusedIdent = unusedName
identToIL (Ident "$__unused") = unusedName
identToIL (Ident name) | name == C.undefined = undefinedName -- Note: done in Printer.hs too
identToIL (Ident name) = properToIL name
identToIL (GenIdent _ _) = internalError "GenIdent in identToIL"

moduleIdentToIL :: Ident -> Text
moduleIdentToIL UnusedIdent = unusedName
moduleIdentToIL (Ident name) | name == C.undefined = undefinedName
moduleIdentToIL (Ident name) = moduleProperToIL name
moduleIdentToIL (GenIdent _ _) = internalError "GenIdent in identToIL"

undefinedName :: Text
undefinedName = "Undefined"

unusedName :: Text
unusedName = "_"

properToIL :: Text -> Text
properToIL name
  | nameIsILReserved name || nameIsILBuiltIn name || prefixIsReserved name = name <> "ˉ"
  | otherwise = T.concatMap identCharToText name

-- | Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = T.singleton c
identCharToText '_' = "_"
identCharToText '\'' = "ʹ"
identCharToText c = "ᵤ" <> T.pack (show (ord c))

moduleProperToIL :: Text -> Text
moduleProperToIL = T.concatMap identCharToText

-- | Checks whether an identifier name is reserved in IL.
nameIsILReserved :: Text -> Bool
nameIsILReserved name =
  name `elem` ilAnyReserved

-- | Checks whether a name matches a built-in value in IL.
nameIsILBuiltIn :: Text -> Bool
nameIsILBuiltIn name =
  name `elem`
    [
    ]

ilAnyReserved :: [Text]
ilAnyReserved =
  concat
    [ ilKeywords
    , ilLiterals
    ]

ilKeywords :: [Text]
ilKeywords =
  [ "break"
  , "case"
  , "chan"
  , "const"
  , "continue"
  , "default"
  , "defer"
  , "else"
  , "fallthrough"
  , "for"
  , "func"
  , "go"
  , "goto"
  , "if"
  , "import"
  , "interface"
  , "map"
  , "package"
  , "range"
  , "return"
  , "select"
  , "struct"
  , "switch"
  , "type"
  , "var"
  ]

moduleIsReserved :: Text -> Bool
moduleIsReserved "Main" = False
moduleIsReserved name = not $ T.isInfixOf "_" name

prefixIsReserved :: Text -> Bool
prefixIsReserved name =
  any (`T.isPrefixOf` name)
    [
    ]

ilLiterals :: [Text]
ilLiterals =
  [ "init"
  , "nil"
  , "int"
  , "int8"
  , "int16"
  , "int32"
  , "int64"
  , "uint"
  , "uint8"
  , "uint16"
  , "uint32"
  , "uint64"
  , "uintptr"
  , "float32"
  , "float64"
  , "complex64"
  , "complex128"
  , "byte"
  , "rune"
  , "string"
  ]

withPrefix :: Text -> Text
withPrefix s = "PS__" <> s

anyType :: Text
anyType = "Any"

dictType :: Text
dictType = "Dict"

arrayType :: Text
arrayType = "[]" <> anyType

int :: Text
int = "int"

float :: Text
float = "float64"

bool :: Text
bool = "bool"

string :: Text
string = "string"

arrayLengthFn :: Text
arrayLengthFn = "Length"

tcoLoop :: Text
tcoLoop = "ᵗᶜloop"

freshName' :: MonadSupply m => m Text
freshName' = do
    name <- freshName
    return $ T.replace "$" "ᵗ" name
