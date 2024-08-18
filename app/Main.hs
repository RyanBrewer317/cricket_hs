module Main (main) where
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Foldable (foldl')
import System.Environment (getArgs)
import Data.Fixed (mod')
import Data.Char (isAlpha, isDigit)
import Data.Either (partitionEithers)

data Pos = Pos String Int Int deriving Show

newtype Parser a = Parser { run :: Maybe String -> Pos -> String -> Either String (a, Pos, String) }

prettyParseError :: Pos -> Maybe String -> String -> String
prettyParseError (Pos srcName line col) expected msg = 
  "Parse error. " ++ msg ++ " in `" ++ srcName ++ "` at " ++ show line ++ ":" ++ show col ++ "."
  ++ case expected of
    Just s -> " Expected " ++ s ++ "."
    Nothing -> ""

prettyRuntimeError :: Pos -> String -> String
prettyRuntimeError (Pos srcName line col) msg = 
  "Runtime error. " ++ msg ++ ". In `" ++ srcName ++ "` at " ++ show line ++ ":" ++ show col ++ "."

(?) :: Parser a -> String -> Parser a
p ? msg = Parser $ \_ pos src -> run p (Just msg) pos src

position :: Parser Pos
position = Parser $ \_ pos src -> Right (pos, pos, src)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \expected (Pos srcName line col) src -> 
  case src of
    c:rest | c == '\n' && p c -> Right (c, Pos srcName (line + 1) 0, rest)
    c:rest | p c -> Right (c, Pos srcName line (col + 1), rest)
    c:_ -> Left $ prettyParseError (Pos srcName line col) expected $ "Unexpected `" ++ c:"`"
    [] -> Left $ prettyParseError (Pos srcName line col) expected "Unexpected end of input"

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

data Syntax = LambdaSyntax Pos String Syntax
            | IdentSyntax Pos String
            | AppSyntax Pos Syntax Syntax
            | IntSyntax Pos Int
            | LetSyntax Pos Bool String Syntax Syntax
            | ObjectSyntax Pos [(String, String, Syntax)]
            | AccessSyntax Pos Syntax String
            | UpdateSyntax Pos Syntax String String Syntax
            | OperatorSyntax Pos Syntax String Syntax
            | StringSyntax Pos String
            | FloatSyntax Pos Float
            deriving Show

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

parseLet :: Parser Syntax
parseLet = do
  p <- position
  _ <- exact "let" ? "an identifier declaration"
  _ <- whitespace
  w <- patternString
  _ <- whitespace0
  (ident, forced) <- case w of
    "force" -> do
      i <- patternString
      return (i, True)
    i -> return (i, False)
  _ <- whitespace0
  _ <- char '='
  val <- parseTerm
  _ <- exact "in"
  _ <- whitespace
  LetSyntax p forced ident val <$> parseTerm

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
  return $ ObjectSyntax p methods

parseParens :: Parser Syntax
parseParens = char '(' *> parseTerm <* char ')'

parseTermNoPostfix :: Parser Syntax
parseTermNoPostfix = do
  _ <- whitespace0
  t <- oneOf [parseParens, parseObject, parseConstantLambda, parseString, parseNum, parseLet, parseIdentOrLambda]
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
    [ AppPostfix <$> position <*> oneOf [parseParens, parseObject] ? "a function call"
    , AccessPostfix <$> position <*> (char '.' >> identString) ? "an object method"
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
            UpdatePostfix p2 self method new -> UpdateSyntax p2 b self method new
            OperatorPostfix p2 op rhs -> OperatorSyntax p2 b op rhs
          ) t args
  _ <- whitespace0
  return out

parseFuncOrObjDecl :: Parser (Either (String, Syntax) String)
parseFuncOrObjDecl = do
  p <- position
  _ <- exact "def" ? "a global declaration"
  _ <- whitespace
  name <- identString
  mb_obj <- possible (whitespace0 >> char '{')
  case mb_obj of
    Just _ -> do
      methods <- parseMethods ? "some object methods"
      _ <- char '}' ? "}"
      return $ Left (name, ObjectSyntax p methods)
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

parseImport :: Parser (Either (String, Syntax) String)
parseImport = do
  _ <- exact "import" ? "an import statement"
  _ <- whitespace
  Right <$> identString ? "a module name" <* whitespace0

parseFile :: Parser ([(String, Syntax)], [String])
parseFile = fmap partitionEithers $ many $ whitespace0 *> oneOf [parseFuncOrObjDecl, parseImport] ? "a declaration" <* whitespace0

data Term = Lambda Pos String Term
          | Ident Pos Int String
          | App Pos Term Term
          | Int Pos Int
          | Builtin Pos String
          | LetForce Pos Term Term
          | Object Pos (Map.Map String Term)
          | Access Pos Term String
          | Update Pos Term String Term
          | Operator Pos Term String Term
          | String Pos String
          | Float Pos Float
          deriving Show

translate :: Int -> Map.Map String Int -> Syntax -> Term
translate index renames t = case t of
      LambdaSyntax p param body ->
        let body2 = translate (index + 1) (Map.insert param index renames) body in
        Lambda p param body2
      IdentSyntax p name ->
        case Map.lookup name renames of
          Just i -> Ident p (index - i - 1) name
          Nothing -> Builtin p name
      AppSyntax p foo bar ->
        let foo2 = translate index renames foo in
        let bar2 = translate index renames bar in
        App p foo2 bar2
      IntSyntax p i -> Int p i
      LetSyntax p True ident val scope ->
        let val2 = translate index renames val in
        let scope2 = translate (index + 1) (Map.insert ident index renames) scope in
        LetForce p val2 scope2
      LetSyntax p False ident val scope -> translate index renames $ AppSyntax p (LambdaSyntax p ident scope) val
      ObjectSyntax p methods ->
        let methods2 = foldr (\(self, method, def) so_far->
                let def2 = translate (index + 1) (Map.insert self index renames) def in
                Map.insert method def2 so_far
              ) Map.empty methods in
        Object p methods2
      AccessSyntax p object method ->
        let object2 = translate index renames object in
        Access p object2 method
      UpdateSyntax p object self method def ->
        let object2 = translate index renames object in
        let def2 = translate (index + 1) (Map.insert self index renames) def in
        Update p object2 method def2
      OperatorSyntax p lhs op rhs ->
        let lhs2 = translate index renames lhs in
        let rhs2 = translate index renames rhs in
        Operator p lhs2 op rhs2
      StringSyntax p s -> String p s
      FloatSyntax p f -> Float p f

translateDecl :: Int -> Map.Map String Int -> (String, Syntax) -> (String, Term)
translateDecl index renames decl = case decl of
  (foo, body) ->
    let body2 = translate (index + 1) (Map.insert foo index renames) body in
    (foo, body2)

translateFile :: [(String, Syntax)]-> Map.Map String Term
translateFile decls =
  let decls2 = foldr (\decl so_far->
          let (name, def) = translateDecl 0 Map.empty decl in
          Map.insert name def so_far
        ) Map.empty decls in
  decls2

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty term = case term of
    Lambda _ x body -> x ++ "-> " ++ pretty body
    Ident _ _ s -> s
    App _ foo bar -> "(" ++ pretty foo ++ ")(" ++ pretty bar ++ ")"
    Int _ i -> show i
    Builtin _ name -> name
    LetForce _ val scope -> "force(" ++ pretty val ++ "): " ++ pretty scope
    Object _ methods -> "{" ++ intercalate ", " (map (\(m,t)->"this."++m++": "++pretty t) $ Map.toList methods) ++ "}"
    Access _ t method -> pretty t ++ "." ++ method
    Update _ t method new -> pretty t ++ " <- this." ++ method ++ ": " ++ pretty new
    Operator _ lhs op rhs -> pretty lhs ++ " " ++ op ++ " " ++ pretty rhs
    String _ s -> "\"" ++ s ++ "\""
    Float _ f -> show f

newtype Env = Env [(Term, Env)] deriving Show

instance Pretty Env where
  pretty (Env env) = case env of
    [(def, def_env)] -> "<" ++ pretty def ++ ", " ++ pretty def_env ++ ">"
    closure:rest -> pretty (Env [closure]) ++ ", " ++ pretty (Env rest)
    [] -> ""

newtype Stack = Stack [(Term, Env)] deriving Show

instance Pretty Stack where
  pretty (Stack l) = pretty (Env l)

normalize :: Map.Map String Term -> Term -> Map.Map String (Map.Map String Term) -> IO (Either String Term)
normalize d t mods = do
  result <- go d t (Stack []) (Env [])
  return $ case result of
    Right (out, _, _) -> Right out
    Left err -> Left err
  where
    go decls term s@(Stack stack) e@(Env env) = do
      -- putStrLn $ pretty term ++ " ; " ++ pretty s ++ "; " ++ pretty e ++ "."
      case term of
        Lambda _ _ body ->
          case stack of
            arg:rest -> go decls body (Stack rest) (Env (arg:env))
            [] -> return $ Right (term, s, e)
        Ident p 0 name ->
          case env of
            (def, new_env):_ -> go decls def s new_env
            [] -> return $ Left $ prettyRuntimeError p $ "Undefined identifer `" ++ name ++ "`"
        Ident p n name -> 
          case env of
            _:rest -> go decls (Ident p (n - 1) name) s (Env rest)
            [] -> return $ Left $ prettyRuntimeError p $ "Undefined identifier `" ++ name ++ "`"
        App _ foo bar ->
          go decls foo (Stack $ (bar, e):stack) e
        Int _ _ -> return $ Right (term, s, e)
        Builtin p "console" -> return $ Right (Object p $ Map.fromList [("write", Builtin p "$write"), ("read", Builtin p "$read")], s, e)
        Builtin p "$write" ->
          case stack of
            [(arg, arg_env)] -> do
              result <- go decls arg (Stack []) arg_env
              case result of
                Right (normal_form, _, _) -> do
                  case normal_form of
                    String _ str -> do
                      putStrLn str
                      return $ Right (Int p 0, Stack [], e)
                    Int _ i -> do
                      print i
                      return $ Right (Int p 0, Stack [], e)
                    Float _ f -> do
                      print f
                      return $ Right (Int p 0, Stack [], e)
                    _ -> return $ Left $ prettyRuntimeError p $ "Can't write `" ++ pretty normal_form ++ "` to the console"
                Left err -> return $ Left err
            _ -> return $ Left $ prettyRuntimeError p $ "`console.write` with wrong number of arguments: " ++ show (length stack)
        Builtin p "$read" ->
          case stack of
            [(arg, arg_env)] -> do
              result <- go decls arg (Stack []) arg_env
              case result of
                Right (normal_form, _, _) -> 
                  case normal_form of
                    Object _ methods | Map.null methods -> do
                      str <- getLine
                      return $ Right (String p str, s, e)
                    _ -> return $ Left $ prettyRuntimeError p $ "Bad argument for `console.read`: `" ++ pretty normal_form ++ "`"
                Left err -> return $ Left err
            _ -> return $ Left $ prettyRuntimeError p $ "`console.read` with wrong number of arguments: " ++ show (length stack)
        Builtin p name ->
          case Map.lookup name decls of
            Just def -> go decls def s (Env [(term, Env [])])
            Nothing ->
              case Map.lookup name mods of
                Just defs -> return $ Right (Object p defs, s, e)
                Nothing -> return $ Left $ prettyRuntimeError p $ "Unknown global `" ++ name ++ "`"
        LetForce _ val scope -> do
          result <- go decls val (Stack []) e
          case result of
            Right (normal_form, _, _) -> go decls scope s (Env $ (normal_form, e):env)
            Left err -> return $ Left err
        Object _ _ -> return $ Right (term, s, e)
        Access p ob method -> do
          result <- go decls ob (Stack []) e
          case result of
            Right (normal_form, _, Env ob_env) -> 
              case normal_form of
                Object _ methods ->
                  case Map.lookup method methods of
                    Just def -> go decls def s (Env $ (normal_form, e):ob_env)
                    Nothing -> return $ Left $ prettyRuntimeError p $ "Unknown object method `" ++ method ++ "` on `" ++ pretty normal_form ++ "`"
                _ -> return $ Left $ prettyRuntimeError p $ "Cannot access a non-object `" ++ pretty (Access p normal_form method) ++ "`"
            Left err -> return $ Left err
        Update p ob method def -> do
          result <- go decls ob (Stack []) e
          case result of
            Right (normal_form, _, _) -> 
              case normal_form of
                Object _ methods ->
                  return $ Right (Object p $ Map.insert method def methods, s, e)
                _ -> return $ Left $ prettyRuntimeError p "Cannot update a non-object"
            Left err -> return $ Left err
        Operator p lhs op rhs -> do
          result <- go decls lhs (Stack []) e
          case result of
            Right (lhs_nf, _, _) -> do
              result2 <- go decls rhs (Stack []) e
              return $ case result2 of
                Right (rhs_nf, _, _) ->
                  case (lhs_nf, rhs_nf) of
                    (Int _ i, Int _ j) ->
                      case op of
                        "+" -> Right (Int p $ i + j, s, e)
                        "-" -> Right (Int p $ i - j, s, e)
                        "*" -> Right (Int p $ i * j, s, e)
                        "/" -> Right (Int p $ div i j, s, e)
                        "%" -> Right (Int p $ mod i j, s, e)
                        _ -> Left $ prettyRuntimeError p $ "Unknown operator `" ++ op ++ "` for integers."
                    (String _ a, String _ b) -> 
                      case op of
                        "+" -> Right (String p $ a ++ b, s, e)
                        _ -> Left $ prettyRuntimeError p $ "Unknown operator `" ++ op ++ "` for strings."
                    (Float _ a, Float _ b) -> 
                      case op of
                        "+" -> Right (Float p $ a + b, s, e)
                        "-" -> Right (Float p $ a - b, s, e)
                        "*" -> Right (Float p $ a * b, s, e)
                        "/" -> Right (Float p $ a / b, s, e)
                        "%" -> Right (Float p $ mod' a b, s, e)
                        _ -> Left $ prettyRuntimeError p $ "Unknown operator `" ++ op ++ "` for floats."
                    _ -> Left $ prettyRuntimeError p $ "Operator `" ++ op ++ "` unknown or applied to values of the wrong types: `" ++ pretty lhs_nf ++ "`, `" ++ pretty rhs ++ "`"
                Left err -> Left err
            Left err -> return $ Left err
        String _ _ -> return $ Right (term, s, e)
        Float _ _ -> return $ Right (term, s, e)

processImport :: String -> IO (Either String (Map.Map String (Map.Map String Term)))
processImport srcName = do
  src <- readFile $ srcName ++ ".ct"
  case run parseFile Nothing (Pos srcName 1 1) src of
    Left err -> return $ Left err
    Right ((decls, imports), _, "") -> do
      eithers <- mapM processImport imports
      let (errors, successes) = partitionEithers eithers
      return $ case errors of
        err:_ -> Left err
        [] ->
          let modules = foldr Map.union Map.empty successes in
          let decls2 = translateFile decls in
          Right $ Map.insert srcName decls2 modules
    Right (_, p, c:_) -> return $ Left $ prettyRuntimeError p $ "Unexpected `" ++ c:"`"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      src <- getLine
      case run parseTerm Nothing (Pos "input" 1 1) src of
        Left err -> putStrLn err
        Right (t, _, "") -> do
          let t2 = translate 0 Map.empty t
          _ <- normalize Map.empty t2 Map.empty
          return ()
        Right (_, pos, c:_) -> putStrLn $ prettyParseError pos Nothing $ "unexpected `" ++ c:"`"
    filename:_ -> do
      src <- readFile filename
      case run parseFile Nothing (Pos filename 1 1) src of
        Left err -> putStrLn err
        Right ((decls, imports), _, "") -> do
          eithers <- mapM processImport imports
          let (errors, successes) = partitionEithers eithers
          case errors of
            err:_ -> putStrLn err
            [] -> do
              let mods = foldr Map.union Map.empty successes
              let decls2 = translateFile decls
              case Map.lookup "main" decls2 of
                Just (Lambda _ _ entry) -> do
                  result <- normalize decls2 entry mods
                  case result  of
                    Left err -> putStrLn err
                    Right _ -> return ()
                _ -> putStrLn $ "Error. No `main` function in `" ++ filename ++ "`."
        Right (_, pos, c:_) -> putStrLn $ prettyParseError pos (Just "a declaration") $ "Unexpected `" ++ c:"`"
