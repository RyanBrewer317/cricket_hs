-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main (main) where
import qualified Data.Map as Map
import Data.List (intercalate, isSuffixOf)
import System.Environment (getArgs)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Common
import Parse
import Translate
import Eval

processImport :: [String] -> EitherIO String (String, String, Syntax)
processImport mod_path_segments = do
  let mod_name = last mod_path_segments
  let mod_path = intercalate "/" mod_path_segments
  let pos = Pos mod_path 1 1
  src <- lift $ readFile $ mod_path ++ ".ct"
  ((decls, imports), pos2, rest) <- EIO $ return $ run parseFile Nothing pos src
  case rest of
    "" -> return ()
    c:_ -> left $ prettyRuntimeError pos2 $ "Unexpected `" ++ c:"`"
  mods <- mapM processImport imports
  let mod_methods = Map.toList $ Map.union (builtins pos) decls
  let ob = ModuleSyntax pos mod_name $ mods ++ map (\(a,b)->('$':mod_name,a,b)) mod_methods
  return ('$':mod_name, mod_name, ob)

removeSuffix :: String -> String -> String
removeSuffix sub s =
  let rev = reverse s in
  if sub `isSuffixOf` s then
    reverse (drop (length sub) rev)
  else
    s

builtins :: Pos -> Map.Map String Syntax
builtins pos = Map.fromList
  [ ("console", ObjectSyntax pos (Just "console")
    [ ("", "write", IdentSyntax pos "#console_write")
    , ("", "read", IdentSyntax pos "#console_read")
    ])
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStr "> "
      hFlush stdout
      src <- getLine
      let pos = Pos "input" 1 1
      case run parseTerm Nothing pos src of
        Left err -> putStrLn err
        Right (t, _, "") -> do
          mb_t <- getEIO $ processImport ["stdlib"]
          let stdlib = case mb_t of
                Right x -> x
                _ -> error "internal error"
          let mod_methods = Map.toList $ Map.insert "main" t $ builtins pos
          let ob = ModuleSyntax pos "input" $
                stdlib : map (\(a,b)->("$input",a,b)) mod_methods
          let t2 = translate "input" 0 Map.empty ob
          res <- normalize $ Access pos t2 "main"
          case res of
            Left err -> putStrLn err
            Right _ -> return ()
        Right (_, p, c:_) ->
          putStrLn $
            prettyParseError p Nothing $ "unexpected `" ++ c:"`"
    filename:_ -> do
      let pos = Pos filename 1 1
      let mod_name = removeSuffix ".ct" filename
      mb_t <- getEIO $ processImport [mod_name]
      case mb_t of
        Left err -> putStrLn err
        Right (_, _, t) -> do
          let t2 = translate mod_name 0 Map.empty t
          let entry = App pos (Access pos t2 "main") (Object pos Nothing Map.empty)
          result <- normalize entry
          case result of
            Left err -> putStrLn err
            Right _ -> return ()
