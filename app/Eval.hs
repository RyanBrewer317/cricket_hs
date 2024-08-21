-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Eval where

import Common
import qualified Data.Map as Map
import Data.Fixed (mod')

prettyRuntimeError :: Pos -> String -> String
prettyRuntimeError (Pos srcName line col) msg =
  "Runtime error. " ++ msg ++ ". In `" ++ srcName ++ "` at "
  ++ show line ++ ":" ++ show col ++ "."

normalize :: Term -> IO (Either String Term)
normalize t = getEIO $ do
  (out, _, _) <- go t (Stack []) (Env [])
  return out
  where
    go term s@(Stack stack) e@(Env env) = do
      case term of
        Lambda _ name body -> do
          case stack of
            (Object p _ methods, ob_env):rest -> go body (Stack rest) (Env $ (Object p (Just name) methods, ob_env):env)
            arg:rest -> go body (Stack rest) (Env (arg:env))
            [] -> return (term, s, e)
        Ident p _ "#console_write" -> do
          case stack of
            [(arg, arg_env)] -> do
              (normal_form, _, _) <- go arg (Stack []) arg_env
              case normal_form of
                String _ str -> do
                  lift $ putStrLn str
                  return (Int p 0, Stack [], e)
                Int _ i -> do
                  lift $ print i
                  return (Int p 0, Stack [], e)
                Float _ f -> do
                  lift $ print f
                  return (Int p 0, Stack [], e)
                _ ->
                  left $ prettyRuntimeError p $
                  "Can't write `" ++ pretty normal_form ++ "` to the console."
            _ ->
              left $ prettyRuntimeError p $
              "`console.write` with wrong number of arguments: "
              ++ show (length stack)
        Ident p _ "#console_read" -> do
          case stack of
            (arg, arg_env):_ -> do
              (normal_form, _, _) <- go arg (Stack []) arg_env
              case normal_form of
                Object _ _ methods | Map.null methods -> do
                  str <- lift getLine
                  return (String p str, s, e)
                _ ->
                  left $ prettyRuntimeError p $
                  "Bad argument for `console.read`: `"
                  ++ pretty normal_form ++ "`"
            [] ->
              return (Lambda p "x"
                  (App p (Ident p 0 "#console_read") (Ident p 0 "x")),
                s, e)
        Ident p 0 name -> do
          case env of
            (def, new_env):_ ->
              go def s new_env
            [] ->
              left $
                prettyRuntimeError p $ "Undefined identifer `" ++ name ++ "`"
        Ident p n name -> do
          case env of
            _:rest -> go (Ident p (n - 1) name) s (Env rest)
            [] ->
              left $ prettyRuntimeError p $
                "Undefined identifier `" ++ name ++ "`"
        App _ foo bar -> do
          go foo (Stack $ (bar, e):stack) e
        Int _ _ -> return (term, s, e)
        LetForce _ name val scope -> do
          (normal_form, _, nf_env) <- go val (Stack []) e
          let nf2 = case normal_form of
                Object p _ methods -> Object p (Just name) methods
                _ -> normal_form
          go scope s (Env $ (nf2, nf_env):env)
        Object p mb_name methods -> do
          return (Object p mb_name $
              Map.map (\(self,v)->(self,InEnv v (Env $ (term,e):env))) methods,
            s, e)
        Access p ob method -> do
          (normal_form, _, ob_env) <- go ob (Stack []) e
          case normal_form of
            Object _ _ methods ->
              case Map.lookup method methods of
                Just (_self, def) -> go def s (Env $ (normal_form, ob_env):env)
                Nothing ->
                  left $ prettyRuntimeError p $
                    "Unknown object method `" ++ method ++ "` on `"
                    ++ pretty normal_form ++ "`"
            _ ->
              left $ prettyRuntimeError p $
              "Cannot access a non-object `"
              ++ pretty (Access p normal_form method) ++ "`"
        Update p ob self method def -> do
          (normal_form, _, _) <- go ob (Stack []) e
          case normal_form of
            Object _ mb_name methods -> return
              (Object p mb_name $ Map.insert method (self, InEnv def e) methods, s, e)
            _ -> left $ prettyRuntimeError p "Cannot update a non-object"
        Operator p lhs op rhs -> do
          (lhs_nf, _, _) <- go lhs (Stack []) e
          (rhs_nf, _, _) <- go rhs (Stack []) e
          EIO $ return $ case (lhs_nf, rhs_nf) of
            (Int _ i, Int _ j) ->
              case op of
                "+" -> Right (Int p $ i + j, s, e)
                "-" -> Right (Int p $ i - j, s, e)
                "*" -> Right (Int p $ i * j, s, e)
                "/" -> Right (Int p $ div i j, s, e)
                "%" -> Right (Int p $ mod i j, s, e)
                _ -> Left $ prettyRuntimeError p $
                      "Unknown operator `" ++ op ++ "` for integers."
            (String _ a, String _ b) ->
              case op of
                "+" -> Right (String p $ a ++ b, s, e)
                _ -> Left $ prettyRuntimeError p $
                      "Unknown operator `" ++ op ++ "` for strings."
            (Float _ a, Float _ b) ->
              case op of
                "+" -> Right (Float p $ a + b, s, e)
                "-" -> Right (Float p $ a - b, s, e)
                "*" -> Right (Float p $ a * b, s, e)
                "/" -> Right (Float p $ a / b, s, e)
                "%" -> Right (Float p $ mod' a b, s, e)
                _ -> Left $ prettyRuntimeError p $
                      "Unknown operator `" ++ op ++ "` for floats."
            _ -> Left $ prettyRuntimeError p $
                  "Operator `" ++ op
                  ++ "` unknown or applied to values of the wrong types: `"
                  ++ pretty lhs_nf ++ "`, `" ++ pretty rhs ++ "`"
        String _ _ -> do
          return (term, s, e)
        Float _ _ -> do
          return (term, s, e)
        InEnv new_t new_env -> go new_t s new_env
