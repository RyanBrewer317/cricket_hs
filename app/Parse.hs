-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Parse where

import Common
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Data.Foldable (foldl')
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>))

newtype Parser a = Parser {
  run :: Maybe String -> Pos -> String -> Either String (a, Pos, String)
}

prettyParseError :: Pos -> Maybe String -> String -> String
prettyParseError (Pos srcName line col) expected msg =
  "Parse error. "
  ++ msg ++ " in `" ++ srcName ++ "` at "
  ++ show line ++ ":" ++ show col ++ "."
  ++ case expected of
    Just s -> " Expected " ++ s ++ "."
    Nothing -> ""

(?) :: Parser a -> String -> Parser a
p ? msg = Parser $ \_ pos src -> run p (Just msg) pos src

position :: Parser Pos
position = Parser $ \_ pos src -> Right (pos, pos, src)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \expected (Pos srcName line col) src ->
  case src of
    c:rest | c == '\n' && p c -> Right (c, Pos srcName (line + 1) 0, rest)
    c:rest | p c -> Right (c, Pos srcName line (col + 1), rest)
    c:_ -> Left $
      prettyParseError (Pos srcName line col) expected $ "Unexpected `" ++ c:"`"
    [] -> Left $
      prettyParseError (Pos srcName line col) expected "Unexpected end of input"

instance Functor Parser where
  fmap f p = Parser $ \expected pos s -> case run p expected pos s of
    Left err -> Left err
    Right (x, pos2, rest) -> Right (f x, pos2, rest)

instance Applicative Parser where
  pure x = Parser $ \_ pos s -> Right (x, pos, s)
  pf <*> pa = Parser $ \expected pos s -> do -- Either monad, not Parser monad
    (f, pos2, rest) <- run pf expected pos s
    (a, pos3, rest2) <- run pa expected pos2 rest
    Right (f a, pos3, rest2)

instance Monad Parser where
  return = pure
  pa >>= f = Parser $ \expected pos s -> do
    (a, pos2, rest) <- run pa expected pos s
    run (f a) expected pos2 rest

char :: Char -> Parser Char
char c = satisfy (==c)

oneOf :: [Parser a] -> Parser a
oneOf [p] = p
oneOf (p:ps) = Parser $ \expected pos s -> case run p expected pos s of
  Left _err -> run (oneOf ps) expected pos s
  Right (x, pos2, rest) -> Right (x, pos2, rest)
oneOf [] = error "oneOf on empty list of parsers"

possible :: Parser a -> Parser (Maybe a)
possible p = oneOf [fmap Just p, return Nothing]

many0 :: Parser a -> Parser [a]
many0 p = Parser $ \expected pos s -> case run p expected pos s of
  Left _ -> Right ([], pos, s)
  Right (x, pos2, rest) -> run ((x:) <$> many0 p) expected pos2 rest

many :: Parser a -> Parser [a]
many p = Parser $ \expected pos s -> do -- Either monad, not Parser monad
  (x, pos2, rest) <- run p expected pos s
  run ((x:) <$> many0 p) expected pos2 rest

exact :: String -> Parser String
exact s = foldr (\c p-> char c *> p) (return ()) s $> s

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy by p = do
  b <- p
  bs <- many0 (by >> p)
  return (b:bs)

sepBy0 :: Parser a -> Parser b -> Parser [b]
sepBy0 by p = oneOf [sepBy by p, return []]

comment :: Parser Char
comment = do
  _ <- exact "//"
  _ <- many0 $ satisfy (/='\n')
  _ <- possible $ char '\n'
  return '\n'

whitespace0 :: Parser [Char]
whitespace0 = many0 $ oneOf [char ' ', char '\n', comment]

whitespace :: Parser [Char]
whitespace = many $ oneOf [char ' ', char '\n', comment]

identString :: Parser String
identString = do
  first <- satisfy isAlpha
  rest <- many0 $ oneOf [satisfy isAlpha, char '_', satisfy isDigit]
  return (first:rest)

patternString :: Parser String
patternString = oneOf
  [ identString
  , do
    _ <- char '_'
    mb_rest <- possible identString
    case mb_rest of
      Just rest -> return $ '_':rest
      Nothing -> return "_"
  ]

escaped :: Parser Char
escaped = do
  _ <- char '\\'
  c <- satisfy $ const True
  case c of
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    _ -> return c

parseIdentOrLambda :: Parser Syntax
parseIdentOrLambda = do
  p <- position
  i <- identString ? "an identifier"
  _ <- whitespace0
  mb_arrow <- possible (exact "->")
  case mb_arrow of
    Just _ -> LambdaSyntax p i <$> parseTerm
    Nothing -> return $ IdentSyntax p i

parseConstantLambda :: Parser Syntax
parseConstantLambda = do
  p <- position
  _ <- char '_' ? "a lambda"
  _ <- whitespace0
  _ <- exact "->"
  LambdaSyntax p "_" <$> parseTerm

parseNum :: Parser Syntax
parseNum = do
  p <- position
  mb_neg <- possible $ char '-' ? "a number"
  whole <- many $ satisfy isDigit
  mb_dec <- possible $ char '.' >> many (satisfy isDigit)
  return $ case (mb_neg, mb_dec) of
    (Just _, Just dec) -> FloatSyntax p $ negate $ read $ whole ++ '.':dec
    (Just _, Nothing) -> IntSyntax p $ negate $ read whole
    (Nothing, Just dec) -> FloatSyntax p $ read $ whole ++ '.':dec
    (Nothing, Nothing) -> IntSyntax p $ read whole

parseString :: Parser Syntax
parseString = do
  p <- position
  _ <- char '"' ? "a string"
  s <- many0 $ oneOf [escaped, satisfy (/='\"')]
  _ <- char '"'
  return $ StringSyntax p s

data LetType = Force | Basic | Back

parseLet :: Parser Syntax
parseLet = do
  p <- position
  _ <- exact "let" ? "an identifier declaration"
  _ <- whitespace -- TODO: should be not(oneOf[satisfy isAlpha, satisfy isDigit, char '_'])
  w <- patternString
  _ <- whitespace0
  (ident, let_type) <- case w of
    "force" -> do
      i <- patternString
      _ <- whitespace0
      _ <- char '='
      return (i, Force)
    i -> do
      _ <- whitespace0
      op <- oneOf [exact "=", exact "<-"]
      case op of
        "=" -> return (i, Basic)
        "<-" -> return (i, Back)
        _ -> error "internal error"
  val <- parseTerm
  _ <- exact "in"
  _ <- whitespace -- TODO: should be not(oneOf[satisfy isAlpha, satisfy isDigit, char '_'])
  scope <- parseTerm
  return $ case let_type of
    Force -> LetForceSyntax p ident val scope
    Basic -> AppSyntax p (LambdaSyntax p ident scope) val
    Back -> AppSyntax p val (LambdaSyntax p ident scope)

parseMethods :: Parser [(String, String, Syntax)]
parseMethods = sepBy0 (char ',') $ do
  _ <- whitespace0
  w <- identString
  mb_dot <- possible (char '.')
  (self, method) <- case mb_dot of
    Just _ -> do
      method <- identString
      return (w, method)
    Nothing -> return ("_", w)
  _ <- whitespace0
  _ <- char ':'
  def <- parseTerm
  _ <- whitespace0
  return (self, method, def)

parseObject :: Parser Syntax
parseObject = do
  p <- position
  _ <- char '{' ? "an object"
  methods <- parseMethods
  _ <- char '}'
  return $ ObjectSyntax p Nothing methods

parseParens :: Parser Syntax
parseParens = char '(' *> parseTerm <* char ')'

parseTermNoPostfix :: Parser Syntax
parseTermNoPostfix = do
  _ <- whitespace0
  t <- oneOf
    [ parseParens
    , parseObject
    , parseConstantLambda
    , parseString
    , parseNum
    , parseLet
    , parseIdentOrLambda
    ]
  _ <- whitespace0
  return t

data Postfix = AppPostfix Pos Syntax
             | AccessPostfix Pos String
             | UpdatePostfix Pos String String Syntax
             | OperatorPostfix Pos String Syntax

parseTerm :: Parser Syntax
parseTerm = do
  t <- parseTermNoPostfix ? "an expression"
  args <- many0 $ oneOf
    [ AppPostfix <$> position <*>
        oneOf [parseParens, parseObject] ? "a function call"
    , AccessPostfix <$> position <*>
        (char '.' >> identString) ? "an object method"
    , do
      p2 <- position
      _ <- whitespace0
      _ <- exact "<-" ? "an object update"
      _ <- whitespace0
      w <- identString ? "an identifier"
      mb_dot <- possible (char '.')
      (self, method) <- case mb_dot of
        Just _ -> do
          method <- identString ? "an object method name"
          return (w, method)
        Nothing -> return ("_", w)
      _ <- whitespace0
      _ <- char ':' ? ":"
      UpdatePostfix p2 self method <$> parseTerm
    , do -- todo: pratt parsing; proper order of operations/infix levels
      p2 <- position
      _ <- whitespace0
      op <- oneOf [exact "+", exact "-"] ? "an operator"
      OperatorPostfix p2 op <$> parseTerm
    ]
  let out = case args of
        [] -> t
        _ -> foldl' (\b a-> case a of
            AppPostfix p2 arg -> AppSyntax p2 b arg
            AccessPostfix p2 method -> AccessSyntax p2 b method
            UpdatePostfix p2 self method new ->
              UpdateSyntax p2 b self method new
            OperatorPostfix p2 op rhs -> OperatorSyntax p2 b op rhs
          ) t args
  _ <- whitespace0
  return out

parseFuncOrObjDecl :: Parser (Either (String, Syntax) [String])
parseFuncOrObjDecl = do
  p <- position
  _ <- exact "def" ? "a global declaration"
  _ <- whitespace -- TODO: should be not(oneOf[satisfy isAlpha, satisfy isDigit, char '_'])
  name <- identString
  mb_obj <- possible (whitespace0 >> char '{')
  case mb_obj of
    Just _ -> do
      methods <- parseMethods ? "some object methods"
      _ <- char '}' ? "}"
      return $ Left (name, ObjectSyntax p (Just name) methods)
    Nothing -> do
      params <- many (do
          _ <- char '(' ? "("
          _ <- whitespace0
          param <- patternString ? "a parameter name"
          _ <- whitespace0
          _ <- char ')' ? ")"
          return param
        ) ? "parameters"
      _ <- whitespace0
      _ <- char ':' ? ":"
      body <- parseTerm
      let body2 = foldr (LambdaSyntax p) body params
      return $ Left (name, body2)

parseImport :: Parser (Either (String, Syntax) [String])
parseImport = do
  _ <- exact "import" ? "an import statement"
  _ <- whitespace -- TODO: should be not(oneOf[satisfy isAlpha, satisfy isDigit, char '_'])
  name <- sepBy (char '/') identString ? "a module name" <* whitespace0
  return $ Right name

parseFile :: Parser (Map.Map String Syntax, [[String]])
parseFile = do
  let parser = many $ whitespace0 *>
        oneOf [parseFuncOrObjDecl, parseImport] ? "a declaration" <* whitespace0
  (decls, imports) <- fmap partitionEithers parser
  return (Map.fromList decls, imports)
