module CodeGen.IL.Optimizer.TCO (tco, tcoLoop) where

import Prelude.Compat

import Data.Text (Text)
import Data.Monoid ((<>))
import Language.PureScript.CoreImp.AST
import Language.PureScript.AST.SourcePos (SourceSpan)
import Safe (headDef, tailSafe)
import CodeGen.IL.Common

import qualified Language.PureScript.Constants as C

tcoLoop :: Text
tcoLoop = "_tco_loop_"  

tco :: AST -> AST -> AST
tco mn = everywhere convert where
  tcoVar :: Text -> Text
  tcoVar arg = "_tco_var_" <> arg <> "_"

  copyVar :: Text -> Text
  copyVar arg = "_copy_" <> arg <> "_"

  tcoDone :: Text
  tcoDone = "_tco_done_"

  tcoResult :: Text
  tcoResult = "_tco_result_"

  convert :: AST -> AST
  convert fn@(Function ss (Just name) args _)
      | isTailRecursive name body'
      = replace (toLoop name outerArgs innerArgs body')
    where
      innerArgs = headDef [] argss
      outerArgs = concat . reverse $ tailSafe argss
      (argss, body', replace) = collectAllFunctionArgs [] id fn
  convert (Assignment ss (Var _ name) fn@(Function _ Nothing _ _))
      | isTailRecursive name body'
      = Assignment ss (Var ss name) (replace (toLoop name outerArgs innerArgs body'))
    where
      innerArgs = headDef [] argss
      outerArgs = concat . reverse $ tailSafe argss
      (argss, body', replace) = collectAllFunctionArgs [] id fn
  convert js = js

  collectAllFunctionArgs :: [[Text]] -> (AST -> AST) -> AST -> ([[Text]], AST, AST -> AST)
  collectAllFunctionArgs allArgs f (Function s1 ident args (Block s2 (body@(Return _ _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (Function s1 ident (map copyVar args) (Block s2 [b]))) body
  collectAllFunctionArgs allArgs f (Function ss ident args body@(Block _ _)) =
    (args : allArgs, body, f . Function ss ident (map copyVar args))
  collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args (Block s3 [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (Return s1 (Function s2 ident (map copyVar args) (Block s3 [b])))) body
  collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args body@(Block _ _))) =
    (args : allArgs, body, f . Return s1 . Function s2 ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailRecursive :: Text -> AST -> Bool
  isTailRecursive ident js = countSelfReferences js > 0 && allInTailPosition js where
    countSelfReferences = everything (+) match where
      match :: AST -> Int
      match (Var _ ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition (Return _ expr)
      | isSelfCall ident expr = countSelfReferences expr == 1
      | otherwise = countSelfReferences expr == 0
    allInTailPosition (While _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (For _ _ js1 js2 body)
      = countSelfReferences js1 == 0 && countSelfReferences js2 == 0 && allInTailPosition body
    allInTailPosition (ForIn _ _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (IfElse _ js1 body el)
      = countSelfReferences js1 == 0 && allInTailPosition body && all allInTailPosition el
    allInTailPosition (Block _ body)
      = all allInTailPosition body
    allInTailPosition (Throw _ js1)
      = countSelfReferences js1 == 0
    allInTailPosition (ReturnNoResult _)
      = True
    allInTailPosition (VariableIntroduction _ _ js1)
      = all ((== 0) . countSelfReferences) js1
    allInTailPosition (Assignment _ _ js1)
      = countSelfReferences js1 == 0
    allInTailPosition (Comment _ _ js1)
      = allInTailPosition js1
    allInTailPosition _
      = False

  toLoop :: Text -> [Text] -> [Text] -> AST -> AST
  toLoop ident outerArgs innerArgs js =
      Block rootSS $
        concatMap (\arg -> [ VariableIntroduction rootSS (tcoVar arg) (Just (Var rootSS (copyVar arg))) ]) (outerArgs ++ innerArgs) ++
        [ Assignment rootSS (Var rootSS $ bool <> " " <> tcoDone) $ BooleanLiteral Nothing False
        , VariableIntroduction rootSS tcoResult Nothing
        , Assignment rootSS (Var rootSS $ auto <> " " <> tcoLoop) (Function rootSS (Just tcoLoop) (outerArgs ++ innerArgs) (Block rootSS [loopify js]))
        , While rootSS (Unary Nothing Not (Var rootSS tcoDone))
            (Block rootSS
              [(Assignment rootSS (Var rootSS tcoResult) (App rootSS (Var rootSS tcoLoop) ((map (Var rootSS . tcoVar) outerArgs) ++ (map (Var rootSS . tcoVar) innerArgs))))])
        , Return rootSS (Var rootSS tcoResult)
        ]
    where
    rootSS = Nothing

    loopify :: AST -> AST
    loopify (Return ss ret)
      | isSelfCall ident ret =
        let
          allArgumentValues = concat $ collectArgs [] ret
        in
          Block ss $
            zipWith (\val arg ->
              Assignment ss (Var ss (tcoVar arg)) val) allArgumentValues outerArgs
            ++ zipWith (\val arg ->
              Assignment ss (Var ss (tcoVar arg)) val) (drop (length outerArgs) allArgumentValues) innerArgs
            ++ [ ReturnNoResult ss ]
      | otherwise = Block ss [ markDone ss, Return ss ret ]
    loopify (ReturnNoResult ss) = Block ss [ markDone ss, ReturnNoResult ss ]
    loopify (While ss cond body) = While ss cond (loopify body)
    loopify (For ss i js1 js2 body) = For ss i js1 js2 (loopify body)
    loopify (ForIn ss i js1 body) = ForIn ss i js1 (loopify body)
    loopify (IfElse ss cond body el) = IfElse ss cond (loopify body) (fmap loopify el)
    loopify (Block ss body) = Block ss (map loopify body)
    loopify other = other

    markDone :: Maybe SourceSpan -> AST
    markDone ss = Assignment ss (Var ss tcoDone) (BooleanLiteral ss True)

    collectArgs :: [[AST]] -> AST -> [[AST]]
    collectArgs acc (App _ fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall :: Text -> AST -> Bool
  isSelfCall ident (App _ (Var _ ident') _) = ident == ident'
  isSelfCall ident (App _ (Indexer _ (Var _ ident') (Var _ "")) _) = ident == ident'
  isSelfCall ident (App _ (Indexer _ (Var _ ident') mn') _) = mn' == mn && ident == ident'
  isSelfCall ident (App _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
