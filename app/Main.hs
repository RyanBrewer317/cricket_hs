{-# LANGUAGE LambdaCase #-}
module Main (main) where
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

exact :: String -> Parser ()
exact s = foldr (\c p-> char c *> p) (return ()) s $> ()

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy by p = do
  b <- p
  bs <- many0 (by >> p)
  return (b:bs)

sepBy0 :: Parser a -> Parser b -> Parser [b]
sepBy0 by p = oneOf [sepBy by p, return []]

whitespace0 :: Parser [Char]
whitespace0 = many0 $ oneOf [char ' ', char '\n']

whitespace :: Parser [Char]
whitespace = many $ oneOf [char ' ', char '\n']

identString :: Parser String
identString = many $ oneOf [lowercase, char '_']

parseIdentOrLambda :: Parser Syntax
parseIdentOrLambda = do
  i <- identString
  _ <- whitespace0
  mb_arrow <- possible (exact "->")
  case mb_arrow of
    Just _ -> LambdaSyntax i <$> parseTerm
    Nothing -> return $ IdentSyntax i

parseInt :: Parser Syntax
parseInt = possible (char '-') >>= \case
  Just _ -> IntSyntax . negate <$> int
  Nothing -> IntSyntax <$> int

parseLet :: Parser Syntax
parseLet = do
  _ <- exact "let"
  _ <- whitespace
  w <- identString
  _ <- whitespace0
  (ident, forced) <- case w of
    "force" -> do
      i <- identString
      return (i, True)
    i -> return (i, False)
  _ <- whitespace0
  _ <- char '='
  val <- parseTerm
  _ <- exact "in"
  _ <- whitespace
  LetSyntax forced ident val <$> parseTerm

parseObject :: Parser Syntax
parseObject = do
  _ <- char '{'
  labels <- sepBy0 (char ',') $ do
    _ <- whitespace0
    self <- identString
    _ <- char '.'
    method <- identString
    _ <- whitespace0
    _ <- char ':'
    def <- parseTerm
    _ <- whitespace0
    return (self, method, def)
  _ <- char '}'
  return $ ObjectSyntax labels

parseParens :: Parser Syntax
parseParens = char '(' *> parseTerm <* char ')'

parseTermNoPostfix :: Parser Syntax
parseTermNoPostfix = do
  _ <- whitespace0
  t <- oneOf [parseParens, parseObject, parseInt, parseLet, parseIdentOrLambda]
  _ <- whitespace0
  return t

data Postfix = AppPostfix Syntax
             | AccessPostfix String
             | UpdatePostfix String String Syntax

parseTerm :: Parser Syntax
parseTerm = do
  t <- parseTermNoPostfix
  args <- many0 $ oneOf [
    fmap AppPostfix parseParens,
    fmap AccessPostfix $ char '.' >> identString,
    do
      _ <- whitespace0
      _ <- exact "<-"
      _ <- whitespace0
      self <- identString
      _ <- char '.'
      method <- identString
      _ <- whitespace0
      _ <- char ':'
      UpdatePostfix self method <$> parseTerm
    ]
  let out = case args of
        [] -> t
        _ -> foldl' (\b a-> case a of
            AppPostfix arg -> AppSyntax b arg
            AccessPostfix method -> AccessSyntax b method
            UpdatePostfix self method new -> UpdateSyntax b self method new
          ) t args
  _ <- whitespace0
  return out

data Syntax = LambdaSyntax String Syntax
            | IdentSyntax String
            | AppSyntax Syntax Syntax
            | IntSyntax Int
            | LetSyntax Bool String Syntax Syntax
            | ObjectSyntax [(String, String, Syntax)]
            | AccessSyntax Syntax String
            | UpdateSyntax Syntax String String Syntax
            deriving Show

translate :: Syntax -> Term
translate term = (\(out,_,_)->out) $ go 0 0 Map.empty Map.empty term
  where
    go :: Int -> Int -> Map.Map String Int -> Map.Map String Int -> Syntax -> (Term, Int, Map.Map String Int)
    go index id_gen ids renames t = case t of
      LambdaSyntax param body ->
        let (body2, id_gen2, ids2) = go (index + 1) id_gen ids (Map.insert param index renames) body in
        (Lambda body2, id_gen2, ids2)
      IdentSyntax name ->
        case Map.lookup name renames of
          Just i -> (Ident (index - i - 1), id_gen, ids)
          Nothing -> (Builtin name, id_gen, ids)
      AppSyntax foo bar ->
        let (foo2, id_gen2, ids2) = go index id_gen ids renames foo in
        let (bar2, id_gen3, ids3) = go index id_gen2 ids2 renames bar in
        (App foo2 bar2, id_gen3, ids3)
      IntSyntax i -> (Int i, id_gen, ids)
      LetSyntax True ident val scope ->
        let (val2, id_gen2, ids2) = go (index + 1) id_gen ids (Map.insert ident index renames) val in
        let (scope2, id_gen3, ids3) = go index id_gen2 ids2 renames scope in
        (LetForce val2 scope2, id_gen3, ids3)
      LetSyntax False ident val scope -> go index id_gen ids renames $ AppSyntax (LambdaSyntax ident scope) val
      ObjectSyntax labels ->
        let (labels2, id_gen2, ids2) = foldr (\(self, method, def) (so_far, idg, is)->
                let (def2, idg2, is2) = go (index + 1) idg is (Map.insert self index renames) def in
                case Map.lookup method is2 of
                  Just i -> (Map.insert i def2 so_far, idg2, is2)
                  Nothing -> (Map.insert idg2 def2 so_far, idg2 + 1, Map.insert method idg2 is2)
              ) (Map.empty, id_gen, ids) labels in
        (Object labels2, id_gen2, ids2)
      AccessSyntax object method ->
        let (object2, id_gen2, ids2) = go index id_gen ids renames object in
        case Map.lookup method ids2 of
          Just i -> (Access object2 i, id_gen2, ids2)
          Nothing -> (Access object2 id_gen2, id_gen2 + 1, Map.insert method id_gen2 ids2)
      UpdateSyntax object self method def ->
        let (object2, id_gen2, ids2) = go index id_gen ids renames object in
        let (def2, id_gen3, ids3) = go (index + 1) id_gen2 ids2 (Map.insert self index renames) def in
        case Map.lookup method ids3 of
          Just i -> (Update object2 i def2, id_gen3, ids3)
          Nothing -> (Update object2 id_gen3 def2, id_gen3 + 1, Map.insert method id_gen3 ids3)

data Term = Lambda Term
          | Ident Int
          | App Term Term
          | Int Int
          | Builtin String
          | LetForce Term Term
          | Object (Map.Map Int Term)
          | Access Term Int
          | Update Term Int Term

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Lambda _body) = "Function"
  pretty (Ident i) = "'" ++ show i
  pretty (App foo bar) = "(" ++ pretty foo ++ ")(" ++ pretty bar ++ ")"
  pretty (Int i) = show i
  pretty (Builtin name) = '$':name
  pretty (LetForce val scope) = "force(" ++ pretty val ++ "): " ++ pretty scope
  pretty (Object _labels) = "Object"
  pretty (Access term label) = pretty term ++ "." ++ show label
  pretty (Update term label new) = pretty term ++ "." ++ show label ++ " <- " ++ pretty new

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
        Builtin name -> error $ "unknown builtin `$" ++ name ++ "`"
        LetForce val scope -> do
          (normal_form, _, _) <- go val (Stack []) e
          go scope s (Env $ (normal_form, e):env)
        Object _ ->
          case stack of
            [] -> return (term, s, e)
            _ -> error "cannot call an object like a function"
        Access ob method -> do
          (normal_form, _, _) <- go ob (Stack []) e
          case normal_form of
            Object labels ->
              case Map.lookup method labels of
                Just def -> go def s (Env $ (normal_form, e):env)
                Nothing -> error "unknown object label"
            _ -> error "cannot access a non-object"
        Update ob method def -> do
          (normal_form, _, _) <- go ob (Stack []) e
          case normal_form of
            Object labels ->
              return (Object $ Map.insert method def labels, s, e)
            _ -> error "cannot update a non-object"

main :: IO ()
main = do
  code <- getLine
  case run parseTerm code of
    Left err -> putStrLn err
    Right (t, "") -> normalize (translate t) $> ()
    Right (_, c:_) -> putStrLn $ "unexpected `" ++ c:"`"