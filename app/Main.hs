module Main (main) where
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.List (intercalate, isSuffixOf)
import Data.Foldable (foldl')
import System.Environment (getArgs)
import Data.Fixed (mod')
import Data.Char (isAlpha, isDigit)
import Data.Either (partitionEithers)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

class Pretty a where
  pretty :: a -> String

data Pos = Pos String Int Int deriving Show

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

prettyRuntimeError :: Pos -> String -> String
prettyRuntimeError (Pos srcName line col) msg =
  "Runtime error. " ++ msg ++ ". In `" ++ srcName ++ "` at "
  ++ show line ++ ":" ++ show col ++ "."

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

data LetType = Basic | Force | Back deriving Show

data Syntax = LambdaSyntax Pos String Syntax
            | IdentSyntax Pos String
            | AppSyntax Pos Syntax Syntax
            | IntSyntax Pos Int
            | LetSyntax Pos LetType String Syntax Syntax
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
    LetSyntax _ Force x val scope ->
      "let force " ++ x ++ " = " ++ pretty val ++ " in " ++ pretty scope
    LetSyntax _ Basic x val scope ->
      "let " ++ x ++ " = " ++ pretty val ++ " in " ++ pretty scope
    LetSyntax _ Back x val scope ->
      "let " ++ x ++ " <- " ++ pretty val ++ " in " ++ pretty scope
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
  _ <- whitespace
  LetSyntax p let_type ident val <$> parseTerm

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
  _ <- whitespace
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
  _ <- whitespace
  name <- sepBy (char '/') identString ? "a module name" <* whitespace0
  return $ Right name

parseFile :: Parser (Map.Map String Syntax, [[String]])
parseFile = do
  let parser = many $ whitespace0 *>
        oneOf [parseFuncOrObjDecl, parseImport] ? "a declaration" <* whitespace0
  (decls, imports) <- fmap partitionEithers parser
  return (Map.fromList decls, imports)

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

translate :: String -> Int -> Map.Map String Int -> Syntax -> Term
translate mod_name index renames t =
  let tr = translate mod_name in
  case t of
    LambdaSyntax p param body ->
      let body2 = tr (index + 1) (Map.insert param index renames) body in
      Lambda p param body2
    IdentSyntax p ('#':name) -> Ident p 0 $ '#':name
    IdentSyntax p name ->
      case Map.lookup name renames of
        Just i -> Ident p (index - i - 1) name
        Nothing ->
          let t2 = AccessSyntax p (IdentSyntax p mod_name) name in
          tr index renames t2
    AppSyntax p foo bar ->
      let foo2 = tr index renames foo in
      let bar2 = tr index renames bar in
      App p foo2 bar2
    IntSyntax p i -> Int p i
    LetSyntax p Force ident val scope ->
      let val2 = tr index renames val in
      let scope2 = tr (index + 1) (Map.insert ident index renames) scope in
      LetForce p ident val2 scope2
    LetSyntax p Basic ident val scope ->
      tr index renames $ AppSyntax p (LambdaSyntax p ident scope) val
    LetSyntax p Back ident val scope ->
      tr index renames $ AppSyntax p val (LambdaSyntax p ident scope)
    ObjectSyntax p mb_name methods ->
      let methods2 = foldr (\(self, method, def) so_far->
              let def2 = tr (index + 1) (Map.insert self index renames) def in
              Map.insert method (self, def2) so_far
            ) Map.empty methods in
      Object p mb_name methods2
    ModuleSyntax p name methods ->
      let methods2 =
            foldr (\(self, method, def) so_far->
              let def2 = translate self (index + 1)
                    (Map.insert self index renames) def in
              Map.insert method (self, def2) so_far
            ) Map.empty methods in
      Object p (Just $ '$':name) methods2
    AccessSyntax p object method ->
      let object2 = tr index renames object in
      Access p object2 method
    UpdateSyntax p object self method def ->
      let object2 = tr index renames object in
      let def2 = tr (index + 1)
            (Map.insert self index renames) def in
      Update p object2 self method def2
    OperatorSyntax p lhs op rhs ->
      let lhs2 = tr index renames lhs in
      let rhs2 = tr index renames rhs in
      Operator p lhs2 op rhs2
    StringSyntax p s -> String p s
    FloatSyntax p f -> Float p f

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

processImport :: [String] -> EitherIO String (String, String, Syntax)
processImport mod_path = do
  let mod_name = last mod_path
  let pos = Pos mod_name 1 1
  src <- lift $ readFile $ intercalate "/" mod_path ++ ".ct"
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
