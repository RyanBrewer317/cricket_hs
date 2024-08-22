-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Common where

import qualified Data.Map as Map
import Data.List (intercalate)

data Pos = Pos String Int Int deriving Show

class Pretty a where
  pretty :: a -> String

data Syntax = LambdaSyntax Pos String Syntax
            | IdentSyntax Pos String
            | AppSyntax Pos Syntax Syntax
            | IntSyntax Pos Int
            | LetForceSyntax Pos String Syntax Syntax
            | ObjectSyntax Pos (Maybe String) [(String, String, Syntax)]
            | AccessSyntax Pos Syntax String
            | UpdateSyntax Pos Syntax String String Syntax
            | OperatorSyntax Pos Syntax String Syntax
            | StringSyntax Pos String
            | FloatSyntax Pos Float
            | ModuleSyntax Pos String [(String, String, Syntax)]
            deriving Show

instance Pretty Syntax where
  pretty stx = case stx of
    LambdaSyntax _ x e -> x ++ "-> " ++ pretty e
    IdentSyntax _ s -> s
    AppSyntax _ f a -> "(" ++ pretty f ++ ")(" ++ pretty a ++ ")"
    IntSyntax _ i -> show i
    LetForceSyntax _ x val scope ->
      "let force " ++ x ++ " = " ++ pretty val ++ " in " ++ pretty scope
    ObjectSyntax _ (Just name) _ -> name
    ObjectSyntax _ _ methods -> "{"
      ++ intercalate ", " (map (\(s,m,e)->s++"."++m++": "++pretty e) methods)
      ++ "}"
    AccessSyntax _ ob method -> pretty ob ++ "." ++ method
    UpdateSyntax _ ob self method val ->
      "(" ++ pretty ob ++ ") <- " ++ self ++ "." ++ method ++ ": " ++ pretty val
    OperatorSyntax _ lhs op rhs -> pretty lhs ++ " " ++ op ++ " " ++ pretty rhs
    StringSyntax _ s -> "\"" ++ s ++ "\""
    FloatSyntax _ f -> show f
    ModuleSyntax _ name _ -> '$':name

data Term = Lambda Pos String Term
          | Ident Pos Int String
          | App Pos Term Term
          | Int Pos Int
          | LetForce Pos String Term Term
          | Object Pos (Maybe String) (Map.Map String (String, Term))
          | Access Pos Term String
          | Update Pos Term String String Term
          | Operator Pos Term String Term
          | String Pos String
          | Float Pos Float
          | InEnv Term Env
          deriving Show

instance Pretty Term where
  pretty term = case term of
    Lambda _ x body -> x ++ "-> " ++ pretty body
    Ident _ _ s -> s
    App _ foo bar -> "(" ++ pretty foo ++ ")(" ++ pretty bar ++ ")"
    Int _ i -> show i
    LetForce _ x val scope ->
      "let force " ++ x ++ " = " ++ pretty val ++ " in " ++ pretty scope
    Object _ (Just name) _ -> name
    Object _ _ methods ->
      "{" ++ intercalate ", "
        (map (\(m,(s,t))->s++"."++m++": "++pretty t) $ Map.toList methods)
      ++ "}"
    Access _ t method -> pretty t ++ "." ++ method
    Update _ t self method new ->
      pretty t ++ " <- " ++ self ++ "." ++ method ++ ": " ++ pretty new
    Operator _ lhs op rhs -> pretty lhs ++ " " ++ op ++ " " ++ pretty rhs
    String _ s -> "\"" ++ s ++ "\""
    Float _ f -> show f
    InEnv t _ -> pretty t

newtype Env = Env [(Term, Env)] deriving Show

instance Pretty Env where
  pretty (Env env) = case env of
    [(def, def_env)] -> "<" ++ pretty def ++ ", " ++ pretty def_env ++ ">"
    closure:rest -> pretty (Env [closure]) ++ ", " ++ pretty (Env rest)
    [] -> ""

newtype Stack = Stack [(Term, Env)] deriving Show

instance Pretty Stack where
  pretty (Stack l) = pretty (Env l)

newtype EitherIO a b = EIO {getEIO :: IO (Either a b)}

instance Functor (EitherIO a) where
  fmap f (EIO eio) = EIO $ do
    e <- eio
    case e of
      Left a -> return $ Left a
      Right b -> return $ Right $ f b

instance Applicative (EitherIO a) where
  pure b = EIO $ return $ return b
  (EIO eiof) <*> (EIO eiob) = EIO $ do
    ef <- eiof
    eb <- eiob
    return $ case (ef, eb) of
      (Left a, _) -> Left a
      (_, Left a) -> Left a
      (Right f, Right b) -> Right $ f b

instance Monad (EitherIO a) where
  return = pure
  (EIO eioa) >>= eiof = EIO $ do
    ea <- eioa
    case ea of
      Left a -> return $ Left a
      Right b -> getEIO $ eiof b

lift :: IO b -> EitherIO a b
lift io = EIO $ Right <$> io

left :: a -> EitherIO a b
left = EIO . return . Left
