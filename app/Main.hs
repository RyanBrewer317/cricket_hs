{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Foldable (foldl')
import Data.Functor (($>))

newtype Parser a = Parser { run :: String -> Either String (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  c:rest -> if p c then Right (c, rest) else Left $ "unexpected `" ++ c:"`"
  [] -> Left "unexpected end of input"

instance Functor Parser where
  fmap f p = Parser $ \s -> case run p s of
    Left err -> Left err
    Right (x, rest) -> Right (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  pf <*> pa = Parser $ \s -> do -- Either monad, not Parser monad
    (f, rest) <- run pf s
    (a, rest2) <- run pa rest
    Right (f a, rest2)

instance Monad Parser where
  return = pure
  pa >>= f = Parser $ \s -> do
    (a, rest) <- run pa s
    run (f a) rest

lowercase :: Parser Char
lowercase = satisfy $ \c-> ord 'a' <= ord c && ord c <= ord 'z'

int :: Parser Int
int = read <$> many (satisfy $ \c -> ord '0' <= ord c && ord c <= ord '9')

char :: Char -> Parser Char
char c = satisfy $ \c2 -> c == c2

oneOf :: [Parser a] -> Parser a
oneOf [p] = p
oneOf (p:ps) = Parser $ \s -> case run p s of
  Left _err -> run (oneOf ps) s
  Right (x, rest) -> Right (x, rest)
oneOf [] = error "oneOf on empty list of parsers"

possible :: Parser a -> Parser (Maybe a)
possible p = oneOf [fmap Just p, return Nothing]

many0 :: Parser a -> Parser [a]
many0 p = Parser $ \s -> case run p s of
  Left _ -> Right ([], s)
  Right (x, rest) -> run ((x:) <$> many0 p) rest

many :: Parser a -> Parser [a]
many p = Parser $ \s -> do -- Either monad, not Parser monad
  (x, rest) <- run p s
  run ((x:) <$> many0 p) rest

-- | Note that this reverses the list of characters, for performance.
whitespace :: Parser [Char]
whitespace = many0 $ oneOf [char ' ', char '\n']

parseLambda :: Parser Syntax
parseLambda = do
  _ <- char '\\'
  _ <- whitespace
  LambdaSyntax <$> (identString <* whitespace <* char '.') <*> parseTerm

identString :: Parser String
identString = many $ oneOf [lowercase, char '_']

parseIdent :: Parser Syntax
parseIdent = IdentSyntax <$> identString

parseInt :: Parser Syntax
parseInt = possible (char '-') >>= \case
  Just _ -> IntSyntax . negate <$> int
  Nothing -> IntSyntax <$> int

parseBuiltin :: Parser Syntax
parseBuiltin = BuiltinSyntax <$> (char '$' *> identString)

parseParens :: Parser Syntax
parseParens = char '(' *> parseTerm <* char ')'

parseTermNoApp :: Parser Syntax
parseTermNoApp = do
  _ <- whitespace
  t <- oneOf [parseParens, parseBuiltin, parseInt, parseIdent, parseLambda]
  _ <- whitespace
  return t

parseTerm :: Parser Syntax
parseTerm = do
  t <- parseTermNoApp
  args <- many0 parseTermNoApp
  let out = case args of
        [] -> t
        _ -> foldl' AppSyntax t args
  _ <- whitespace
  return out

data Syntax = LambdaSyntax String Syntax
            | IdentSyntax String
            | AppSyntax Syntax Syntax
            | IntSyntax Int
            | BuiltinSyntax String
            deriving Show

debruijn :: Syntax -> Either String Term
debruijn = go 0 Map.empty
  where
    go :: Int -> Map.Map String Int -> Syntax -> Either String Term
    go index renames t = case t of
      LambdaSyntax param body -> Lambda <$> go (index + 1) (Map.insert param index renames) body
      IdentSyntax name ->
        case Map.lookup name renames of
          Just i -> Right $ Ident (index - i - 1)
          Nothing -> Left $ "unknown identifier `" ++ name ++ "`"
      AppSyntax foo bar -> App <$> go index renames foo <*> go index renames bar
      IntSyntax i -> Right $ Int i
      BuiltinSyntax name -> Right $ Builtin name

data Term = Lambda Term
          | Ident Int
          | App Term Term
          | Int Int
          | Builtin String

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Lambda _body) = "function"
  pretty (Ident i) = "'" ++ show i
  pretty (App foo bar) = "(" ++ pretty foo ++ ")(" ++ pretty bar ++ ")"
  pretty (Int i) = show i
  pretty (Builtin name) = '$':name

newtype Env = Env [(Term, Env)]

instance Pretty Env where
  pretty (Env [(def, def_env)]) = "<" ++ pretty def ++ ", " ++ pretty def_env ++ ">"
  pretty (Env (closure:rest)) = pretty (Env [closure]) ++ ", " ++ pretty (Env rest)
  pretty (Env []) = ""

newtype Stack = Stack [(Term, Env)]

instance Pretty Stack where
  pretty (Stack l) = pretty (Env l)

normalize :: Term -> IO Term
normalize t = go t (Stack []) (Env []) >>= \(out, _, _) -> return out
  where
    go term s@(Stack stack) e@(Env env) = do
      -- putStrLn $ pretty term ++ " ; " ++ pretty s ++ "; " ++ pretty e ++ "."
      case term of
        Lambda body ->
          case stack of
            arg:rest -> go body (Stack rest) (Env (arg:env))
            [] -> return (term, s, e)
        Ident 0 ->
          case env of
            (def, new_env):_ -> go def s new_env
            [] -> error "undefined identifer"
        Ident n -> go (Ident $ n - 1) s (Env $ tail env)
        App foo bar ->
          go foo (Stack $ (bar, e):stack) e
        Int _ ->
          case stack of
            [] -> return (term, s, e)
            _ -> error "cannot call an integer like a function"
        Builtin "print" ->
          case stack of
            [(arg, arg_env)] -> do
              (normal_form, _, _) <- go arg (Stack []) arg_env
              putStrLn $ pretty normal_form
              return (Int 0, Stack [], e)
            _ -> error $ "`$print` with wrong number of arguments: " ++ show (length stack)
        Builtin "seq" -> 
          case stack of
            [(arg1, arg1_env), (arg2, arg2_env)] -> do
              (normal_form, _, _) <- go arg1 (Stack []) arg1_env
              go arg2 (Stack [(normal_form, Env [])]) arg2_env
            _ -> error "`$seq` with wrong number of arguments"
        Builtin name -> error $ "unknown builtin `$" ++ name ++ "`"

main :: IO ()
main = do
  code <- getLine
  case run parseTerm code of
    Left err -> putStrLn err
    Right (t, "") ->
      case debruijn t of
        Left err -> putStrLn err
        Right t2 -> normalize t2 $> ()
    Right (_, c:_) -> putStrLn $ "unexpected `" ++ c:"`"