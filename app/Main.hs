{-# LANGUAGE LambdaCase #-}
module Main (main) where
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Foldable (foldl')
import System.Environment (getArgs)
import Data.Fixed (mod')
import Data.Char (isAlpha, isDigit)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug

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

data Syntax = LambdaSyntax String Syntax
            | IdentSyntax String
            | AppSyntax Syntax Syntax
            | IntSyntax Int
            | LetSyntax Bool String Syntax Syntax
            | ObjectSyntax [(String, String, Syntax)]
            | AccessSyntax Syntax String
            | UpdateSyntax Syntax String String Syntax
            | OperatorSyntax Syntax String Syntax
            | StringSyntax String
            | FloatSyntax Float
            deriving Show

parseIdentOrLambda :: Parser Syntax
parseIdentOrLambda = do
  i <- identString
  _ <- whitespace0
  mb_arrow <- possible (exact "->")
  case mb_arrow of
    Just _ -> LambdaSyntax i <$> parseTerm
    Nothing -> return $ IdentSyntax i

parseConstantLambda :: Parser Syntax
parseConstantLambda = do
  _ <- char '_'
  _ <- whitespace0
  _ <- exact "->"
  LambdaSyntax "_" <$> parseTerm

parseNum :: Parser Syntax
parseNum = do
  mb_neg <- possible $ char '-'
  whole <- many $ satisfy isDigit
  mb_dec <- possible $ char '.' >> many (satisfy isDigit)
  return $ case (mb_neg, mb_dec) of
    (Just _, Just dec) -> FloatSyntax $ negate $ read $ whole ++ '.':dec
    (Just _, Nothing) -> IntSyntax $ negate $ read whole
    (Nothing, Just dec) -> FloatSyntax $ read $ whole ++ '.':dec
    (Nothing, Nothing) -> IntSyntax $ read whole

parseString :: Parser Syntax
parseString = do
  _ <- char '"'
  s <- many0 $ oneOf [escaped, satisfy (/='\"')]
  _ <- char '"'
  return $ StringSyntax s

parseLet :: Parser Syntax
parseLet = do
  _ <- exact "let"
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
  LetSyntax forced ident val <$> parseTerm

parseObject :: Parser Syntax
parseObject = do
  _ <- char '{'
  labels <- sepBy0 (char ',') $ do
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
  _ <- char '}'
  return $ ObjectSyntax labels

parseParens :: Parser Syntax
parseParens = char '(' *> parseTerm <* char ')'

parseTermNoPostfix :: Parser Syntax
parseTermNoPostfix = do
  _ <- whitespace0
  t <- oneOf [parseParens, parseObject, parseConstantLambda, parseString, parseNum, parseLet, parseIdentOrLambda]
  _ <- whitespace0
  return t

data Postfix = AppPostfix Syntax
             | AccessPostfix String
             | UpdatePostfix String String Syntax
             | OperatorPostfix String Syntax

parseTerm :: Parser Syntax
parseTerm = do
  t <- parseTermNoPostfix
  args <- many0 $ oneOf
    [ AppPostfix <$> oneOf [parseParens, parseObject]
    , fmap AccessPostfix $ char '.' >> identString
    , do
      _ <- whitespace0
      _ <- exact "<-"
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
      UpdatePostfix self method <$> parseTerm
    , do -- todo: pratt parsing; proper order of operations/infix levels
      _ <- whitespace0
      op <- oneOf [exact "+", exact "-"]
      OperatorPostfix op <$> parseTerm
    ]
  let out = case args of
        [] -> t
        _ -> foldl' (\b a-> case a of
            AppPostfix arg -> AppSyntax b arg
            AccessPostfix method -> AccessSyntax b method
            UpdatePostfix self method new -> UpdateSyntax b self method new
            OperatorPostfix op rhs -> OperatorSyntax b op rhs
          ) t args
  _ <- whitespace0
  return out

data DeclSyntax = FuncSyntax String Syntax

parseFuncDecl :: Parser (Either DeclSyntax a)
parseFuncDecl = do
  _ <- whitespace0
  _ <- exact "def"
  _ <- whitespace
  name <- identString
  params <- many $ do
    _ <- char '('
    _ <- whitespace0
    param <- patternString
    _ <- whitespace0
    _ <- char ')'
    return param
  _ <- whitespace0
  _ <- char ':'
  body <- parseTerm
  let body2 = foldr LambdaSyntax body params
  return $ Left $ FuncSyntax name body2

parseImport :: Parser (Either a String)
parseImport = do
  _ <- whitespace0
  _ <- exact "import"
  _ <- whitespace
  Right <$> identString <* whitespace0

parseFile :: Parser ([DeclSyntax], [String])
parseFile = fmap partitionEithers $ many $ oneOf [parseFuncDecl, parseImport]

data Term = Lambda String Term
          | Ident Int String
          | App Term Term
          | Int Int
          | Builtin String
          | LetForce Term Term
          | Object (Map.Map Int Term)
          | Access Term Int
          | Update Term Int Term
          | Operator Term String Term
          | String String
          | Float Float
          deriving Show

data Module = Module [Decl] (Map.Map String Int)

translate :: Int -> Int -> Map.Map String Int -> Map.Map String Int -> Syntax -> (Term, Int, Map.Map String Int)
translate index id_gen ids renames t = case t of
      LambdaSyntax param body ->
        let (body2, id_gen2, ids2) = translate (index + 1) id_gen ids (Map.insert param index renames) body in
        (Lambda param body2, id_gen2, ids2)
      IdentSyntax name ->
        case Map.lookup name renames of
          Just i -> (Ident (index - i - 1) name, id_gen, ids)
          Nothing -> (Builtin name, id_gen, ids)
      AppSyntax foo bar ->
        let (foo2, id_gen2, ids2) = translate index id_gen ids renames foo in
        let (bar2, id_gen3, ids3) = translate index id_gen2 ids2 renames bar in
        (App foo2 bar2, id_gen3, ids3)
      IntSyntax i -> (Int i, id_gen, ids)
      LetSyntax True ident val scope ->
        let (val2, id_gen2, ids2) = translate index id_gen ids renames val in
        let (scope2, id_gen3, ids3) = translate (index + 1) id_gen2 ids2 (Map.insert ident index renames) scope in
        (LetForce val2 scope2, id_gen3, ids3)
      LetSyntax False ident val scope -> translate index id_gen ids renames $ AppSyntax (LambdaSyntax ident scope) val
      ObjectSyntax labels ->
        let (labels2, id_gen2, ids2) = foldr (\(self, method, def) (so_far, idg, is)->
                let (def2, idg2, is2) = translate (index + 1) idg is (Map.insert self index renames) def in
                case Map.lookup method is2 of
                  Just i -> (Map.insert i def2 so_far, idg2, is2)
                  Nothing -> (Map.insert idg2 def2 so_far, idg2 + 1, Map.insert method idg2 is2)
              ) (Map.empty, id_gen, ids) labels in
        (Object labels2, id_gen2, ids2)
      AccessSyntax object method ->
        let (object2, id_gen2, ids2) = translate index id_gen ids renames object in
        case Map.lookup method ids2 of
          Just i -> (Access object2 i, id_gen2, ids2)
          Nothing -> (Access object2 id_gen2, id_gen2 + 1, Map.insert method id_gen2 ids2)
      UpdateSyntax object self method def ->
        let (object2, id_gen2, ids2) = translate index id_gen ids renames object in
        let (def2, id_gen3, ids3) = translate (index + 1) id_gen2 ids2 (Map.insert self index renames) def in
        case Map.lookup method ids3 of
          Just i -> (Update object2 i def2, id_gen3, ids3)
          Nothing -> (Update object2 id_gen3 def2, id_gen3 + 1, Map.insert method id_gen3 ids3)
      OperatorSyntax lhs op rhs ->
        let (lhs2, id_gen2, ids2) = translate index id_gen ids renames lhs in
        let (rhs2, id_gen3, ids3) = translate index id_gen2 ids2 renames rhs in
        (Operator lhs2 op rhs2, id_gen3, ids3)
      StringSyntax s -> (String s, id_gen, ids)
      FloatSyntax f -> (Float f, id_gen, ids)

data Decl = Func String Term

translateDecl :: Int -> Int -> Map.Map String Int -> Map.Map String Int -> DeclSyntax -> (Decl, Int, Map.Map String Int)
translateDecl index id_gen ids renames decl = case decl of
  FuncSyntax foo body ->
    let (body2, id_gen2, ids2) = translate (index + 1) id_gen ids (Map.insert foo index renames) body in
    (Func foo body2, id_gen2, ids2)

translateFile :: [DeclSyntax]-> ([Decl], Map.Map String Int)
translateFile decls =
  let (decls2, _, out_ids) = foldr (\decl (so_far, id_gen, ids)->
          let (decl2, id_gen2, ids2) = translateDecl 0 id_gen ids Map.empty decl in
          (decl2:so_far, id_gen2, ids2)
        ) ([], 0, Map.empty) decls in
  (decls2, out_ids)

class Pretty a where
  pretty :: Map.Map String Int -> a -> String

instance Pretty Term where
  pretty ids (Lambda x body) = x ++ "-> " ++ pretty ids body
  pretty _ (Ident _ s) = s
  pretty ids (App foo bar) = "(" ++ pretty ids foo ++ ")(" ++ pretty ids bar ++ ")"
  pretty _ (Int i) = show i
  pretty _ (Builtin name) = name
  pretty ids (LetForce val scope) = "force(" ++ pretty ids val ++ "): " ++ pretty ids scope
  pretty ids (Object labels) = "{" ++ intercalate ", " (map (\(i,t)->"this."++idToMethod ids i++": "++pretty ids t) $ Map.toList labels) ++ "}"
  pretty ids (Access term label) = pretty ids term ++ "." ++ idToMethod ids label
  pretty ids (Update term label new) = pretty ids term ++ " <- this." ++ idToMethod ids label ++ ": " ++ pretty ids new
  pretty ids (Operator lhs op rhs) = pretty ids lhs ++ " " ++ op ++ " " ++ pretty ids rhs
  pretty _ (String s) = "\"" ++ s ++ "\""
  pretty _ (Float f) = show f

newtype Env = Env [(Term, Env)] deriving Show

instance Pretty Env where
  pretty ids (Env [(def, def_env)]) = "<" ++ pretty ids def ++ ", " ++ pretty ids def_env ++ ">"
  pretty ids (Env (closure:rest)) = pretty ids (Env [closure]) ++ ", " ++ pretty ids (Env rest)
  pretty _ (Env []) = ""

newtype Stack = Stack [(Term, Env)] deriving Show

instance Pretty Stack where
  pretty ids (Stack l) = pretty ids (Env l)

swapObjectLabels :: [(Int, Int)] -> Term -> Term
swapObjectLabels swaps t = 
  let swap = swapObjectLabels $ Debug.trace (show swaps) swaps in
  case t of
    Lambda x body -> Lambda x $ swap body
    App foo bar -> App (swap foo) (swap bar)
    LetForce val scope -> LetForce (swap val) (swap scope)
    Object labels -> Object $ Map.mapKeys (\i->fromMaybe i $ lookup i swaps) labels
    Access ob i -> Access (swap ob) (fromMaybe i $ lookup i swaps)
    Update ob i v -> Update (swap ob) (fromMaybe i $ lookup i swaps) (swap v)
    Operator lhs op rhs -> Operator (swap lhs) op (swap rhs)
    _ -> t

swapObjectLabelsInDecls :: [(Int, Int)] -> [Decl] -> [Decl]
swapObjectLabelsInDecls swaps decls = case decls of
  Func name body : rest -> Func name (swapObjectLabels swaps body) : swapObjectLabelsInDecls swaps rest
  [] -> []

swapSet :: Map.Map String Int -> Map.Map String Int -> [(Int, Int)]
swapSet ids1 ids2 = Map.elems $ Map.mapWithKey (\s i->(i, fromMaybe i $ Map.lookup s ids2)) ids1

modToObject :: Map.Map String Int -> Module -> Map.Map Int Term
modToObject ids (Module decls _) = 
  foldr (\(Func name def) so_far->
    case Map.lookup name ids of
      Just i -> Map.insert i def so_far
      Nothing -> so_far
  ) Map.empty decls

idToMethod :: Map.Map String Int -> Int -> String
idToMethod ids i =
  case Map.toList $ Map.filter (==i) ids of
    [(name, _)] -> name
    _ -> error "internal error"

normalize :: [Decl] -> Term -> Map.Map String Int -> Map.Map String Module -> IO Term
normalize d t ids mods = go d t (Stack []) (Env []) >>= \(out, _, _) -> return out
  where
    go decls term s@(Stack stack) e@(Env env) = do
      -- putStrLn $ pretty term ++ " ; " ++ pretty s ++ "; " ++ pretty e ++ "."
      case term of
        Lambda _ body ->
          case stack of
            arg:rest -> go decls body (Stack rest) (Env (arg:env))
            [] -> return (term, s, e)
        Ident 0 name ->
          case env of
            (def, new_env):_ -> go decls def s new_env
            [] -> error $ "undefined identifer `" ++ name ++ "`"
        Ident n name -> go decls (Ident (n - 1) name) s (Env $ tail env)
        App foo bar ->
          go decls foo (Stack $ (bar, e):stack) e
        Int _ ->
          case stack of
            [] -> return (term, s, e)
            _ -> error "cannot call an integer like a function"
        Builtin "console" -> do
          case (Map.lookup "write" (Debug.trace (show ids) ids), Map.lookup "read" ids) of
            (Just write_id, Just read_id) -> return (Object $ Map.fromList [(write_id, Builtin "$write"), (read_id, Builtin "$read")], s, e)
            (Just write_id, Nothing) -> return (Object $ Map.fromList [(write_id, Builtin "$write")], s, e)
            (Nothing, Just read_id) -> return (Object $ Map.fromList [(read_id, Builtin "$read")], s, e)
            (Nothing, Nothing) -> return (Object Map.empty, s, e)
        Builtin "$write" ->
          case stack of
            [(arg, arg_env)] -> do
              (normal_form, _, _) <- go decls arg (Stack []) arg_env
              case normal_form of
                String str -> putStrLn str
                Int i -> print i
                Float f -> print f
                _ -> error $ "can't write `" ++ pretty ids normal_form ++ "` to the console"
              return (Int 0, Stack [], e)
            _ -> error $ "`console.write` with wrong number of arguments: " ++ show (length stack)
        Builtin "$read" ->
          case stack of
            [(arg, arg_env)] -> do
              (normal_form, _, _) <- go decls arg (Stack []) arg_env
              case normal_form of
                Object labels | Map.null labels -> do
                  str <- getLine
                  return (String str, s, e)
                _ -> error $ "bad argument for `console.read`: `" ++ pretty ids normal_form ++ "`"
            _ -> error $ "`console.read` with wrong number of arguments: " ++ show (length stack)
        Builtin name ->
          case fetch name decls of
            Just def -> go decls def s (Env [(term, Env [])])
            Nothing -> 
              case Map.lookup name mods of
                Just m -> return (Object $ modToObject ids m, s, e)
                Nothing -> error $ "unknown global `" ++ name ++ "`"
        LetForce val scope -> do
          (normal_form, _, _) <- go decls val (Stack []) e
          go decls scope s (Env $ (normal_form, e):env)
        Object _ ->
          case stack of
            [] -> return (term, s, e)
            _ -> error $ "cannot call an object like a function: `" ++ pretty ids term ++ "`"
        Access ob method -> do
          (normal_form, _, Env ob_env) <- go decls ob (Stack []) e
          case normal_form of
            Object labels ->
              case Map.lookup method labels of
                Just def -> go decls def s (Env $ (normal_form, e):ob_env)
                Nothing -> error $ "unknown object label `" ++ idToMethod ids method ++ "` on `" ++ pretty ids normal_form ++ "`"
            _ -> error $ "cannot access a non-object `" ++ pretty ids (Access normal_form method) ++ "`"
        Update ob method def -> do
          (normal_form, _, _) <- go decls ob (Stack []) e
          case normal_form of
            Object labels ->
              return (Object $ Map.insert method def labels, s, e)
            _ -> error "cannot update a non-object"
        Operator lhs op rhs -> do
          (lhs_nf, _, _) <- go decls lhs (Stack []) e
          (rhs_nf, _, _) <- go decls rhs (Stack []) e
          case (lhs_nf, op, rhs_nf) of
            (Int i, "+", Int j) -> return (Int $ i + j, s, e)
            (Int i, "-", Int j) -> return (Int $ i - j, s, e)
            (Int i, "*", Int j) -> return (Int $ i * j, s, e)
            (Int i, "/", Int j) -> return (Int $ div i j, s, e)
            (Int i, "%", Int j) -> return (Int $ mod i j, s, e)
            (String a, "+", String b) -> return (String $ a ++ b, s, e)
            (Float a, "+", Float b) -> return (Float $ a + b, s, e)
            (Float a, "-", Float b) -> return (Float $ a - b, s, e)
            (Float a, "*", Float b) -> return (Float $ a * b, s, e)
            (Float a, "/", Float b) -> return (Float $ a / b, s, e)
            (Float a, "%", Float b) -> return (Float $ mod' a b, s, e)
            _ -> error $ "operator `" ++ op ++ "` unknown or applied to values of the wrong types: `" ++ pretty ids lhs_nf ++ "`, `" ++ pretty ids rhs ++ "`"
        String _ -> do
          case stack of
            [] -> return (term, s, e)
            _ -> error "cannot call a string like a function"
        Float _ -> do
          case stack of
            [] -> return (term, s, e)
            _ -> error "cannot call a float like a function"

fetch :: String -> [Decl] -> Maybe Term
fetch s (Func name body:_) | s == name = Just body
fetch s (_:rest) = fetch s rest
fetch _ [] = Nothing

processImport :: String -> IO (Either String (Map.Map String Module))
processImport srcName = do
  src <- readFile $ srcName ++ ".ct"
  case run parseFile src of
    Left err -> return $ Left err
    Right ((decls, imports), "") -> do
      eithers <- mapM processImport imports
      let (errors, successes) = partitionEithers eithers
      return $ case errors of
        err:_ -> Left err
        [] ->
          let modules = foldr Map.union Map.empty successes in
          let (decls2, ids) = translateFile decls in
          Right $ Map.insert srcName (Module decls2 ids) modules
    Right (_, c:_) -> return $ Left $ "unexpected `" ++ c:"`"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      code <- getLine
      case run parseTerm code of
        Left err -> putStrLn err
        Right (t, "") -> do
          let (t2, _, ids) = translate 0 0 Map.empty Map.empty t
          _ <- normalize [] t2 ids Map.empty
          return ()
        Right (_, c:_) -> putStrLn $ "unexpected `" ++ c:"`"
    filename:_ -> do
      code <- readFile filename
      case run parseFile code of
        Left err -> putStrLn err
        Right ((decls, imports), "") -> do
          eithers <- mapM processImport imports
          let (errors, successes) = partitionEithers eithers
          let mods = case errors of
                err:_ -> error err
                [] -> foldr Map.union Map.empty successes
          let (decls2, ids) = translateFile decls
          let mods2 = Map.map (\(Module decls3 ids2)->Module (swapObjectLabelsInDecls (swapSet ids2 ids) decls3) ids2) mods
          case fetch "main" decls2 of
            Just (Lambda _ entry) -> normalize decls2 entry ids mods2 $> ()
            _ -> error "no main function"
        Right (_, c:_) -> putStrLn $ "unexpected `" ++ c:"`"
