{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Parser where

import Control.Applicative
import Data.Foldable (asum)
import Data.Char

data SchemeValue
  = SchemeBool Bool
  | SchemeInteger Integer
  | SchemeDouble Double
  | SchemeString String
  | SchemeSymbol String
  | SchemeQuote String
  | SchemeVariable String
  | SchemeIf SchemeValue SchemeValue SchemeValue
  | SchemeCond [(SchemeValue,SchemeValue)]
  | SchemeDefinition Definition
  | SchemeLambda Lambda
  | SchemeFunctionCall String [SchemeValue]
  | SchemeLambdaCall SchemeValue [SchemeValue]
  deriving Eq

instance Show SchemeValue where
  show (SchemeBool       v) = show v
  show (SchemeInteger    v) = show v
  show (SchemeDouble     v) = show v
  show (SchemeString     v) = show v
  show (SchemeSymbol     v) = "'" ++ v
  show (SchemeQuote      v) = "'" ++ v
  show (SchemeVariable   v) = "var " ++ show v
  show (SchemeLambda     v) = show v
  show (SchemeDefinition v) = show v
  show (SchemeIf p t f) =
    "if (" ++ show p ++ ") then: "
    ++ show t
    ++ " else: " ++ show f
  show (SchemeCond conds) =
    "cond " ++ show conds
  show (SchemeFunctionCall f args)  =
    "call " ++ show f ++ " with args " ++ show args
  show (SchemeLambdaCall lamb args) =
    "call (" ++ show lamb ++ ") with args " ++ show args

data Definition = Definition String Lambda
  deriving (Eq, Show)

data Lambda = Lambda [String] [Definition] SchemeValue
  deriving (Eq, Show)

-- TODO: change to String -> Maybe (String, a) ???
newtype Parser a = Parser {runParser :: String -> Maybe (a,String)}

instance Functor Parser where
  fmap f (Parser p) = Parser \inp -> do
    (x, inp') <- p inp
    return (f x, inp')

instance Applicative Parser where
  pure x = Parser \inp -> Just (x, inp)
  Parser f <*> Parser p = Parser \inp -> do
    (f, inp')  <- f inp
    (x, inp'') <- p inp'
    return (f x, inp'')

instance Alternative Parser where
  empty = Parser $ const  Nothing
  Parser p1 <|> Parser p2 = Parser \inp ->
    case p1 inp of
      Nothing -> p2 inp
      out     -> out

instance Monad Parser where
  Parser px >>= f = Parser \inp -> do
    (x, inp') <- px inp
    runParser (f x) inp'


charP :: (Char -> Bool) -> Parser Char
charP p = Parser \case
  (x:xs) | p x -> Just (x, xs)
  _            -> Nothing

stringP :: String -> Parser String
stringP = traverse \x -> charP (==x)

boolP :: Parser SchemeValue
boolP = trueP <|> falseP
  where
    trueP  = SchemeBool True  <$ stringP "#t"
    falseP = SchemeBool False <$ stringP "#f"


digitsP :: Parser String
digitsP = some $ charP isDigit

negDigitsP :: Parser String
negDigitsP = do
  s  <- charP (=='-')
  ds <- digitsP
  return (s:ds)

integerP :: Parser SchemeValue
integerP = do
  ds <- negDigitsP <|> digitsP
  return $ SchemeInteger $ read ds


doubleP :: Parser SchemeValue
doubleP = do
  n <- negDigitsP <|> digitsP
  _ <- charP (=='.')
  m <- digitsP
  return $ SchemeDouble $ read $ n++'.':m


-- no escape support
schemeStringP :: Parser SchemeValue
schemeStringP = do
  charP (=='"')
  x <- many $ charP (/='"')
  charP (=='"')
  return $ SchemeString x

ws :: Parser String
ws = many $ charP isSpace

bracket :: Parser a -> Parser a
bracket p = do
  ws
  charP (=='(')
  ws
  x <- p
  ws
  charP (==')')
  ws
  return x

isWord :: String -> Parser String
isWord s = ws *> stringP s

ifP :: Parser SchemeValue
ifP = bracket $ SchemeIf
  <$> do isWord "if" *> schemeP
  <*> schemeP
  <*> schemeP

forbidden :: [String]
forbidden = ["if", "cond", "define", "lambda"]

notKeyword :: String -> Bool
notKeyword x =  x `notElem` forbidden

varP :: Parser String
varP = do
  ws

  x <- charP isLetter
  ys <- many $ charP $ liftA2 (||) isLetter isDigit

  ws
  let s = x:ys in
    if notKeyword s
    then return s
    else empty

condP :: Parser SchemeValue
condP = bracket do
    isWord "cond"
    vs <- some pair
    return $ SchemeCond vs
  where
    pair = bracket $ (,)
      <$> schemeP
      <*> schemeP


symbolP :: Parser SchemeValue
symbolP = do
  xs <- some $ charP isLetter
  ys <- many $ charP $ liftA2 (||) isLetter isDigit
  return $ SchemeSymbol (xs++ys)



quoteP :: Parser SchemeValue
quoteP = do
  charP (=='\'')
  SchemeQuote <$> do bracketed <|> oneVal
  where

    bracketed = do
      ws
      open   <- charP (=='(')
      middle <- many $ charP (/=')')
      close  <- charP (==')')
      pure $ open : middle ++ [close]

    oneVal = some $ charP (/=' ')


defP :: Parser Definition
defP = bracket do
  isWord "define"
  ws
  let idents = Right <$> bracket (some varP)
      singleIdent = Left <$> varP

  params <- singleIdent <|> idents
  ws
  defs <- many defP
  body <- schemeP
  ws

  pure $ case params of
    Left single -> Definition single $ Lambda [] defs body

    Right [] -> error "the impossible has happened"
    Right (x:xs) -> Definition x $ Lambda xs defs body


lambdaP :: Parser SchemeValue
lambdaP = bracket do
  isWord "lambda"
  params <- (:[]) <$> syn <|> bracket do many syn
  defs   <- many defP
  body   <- schemeP <|> bracket schemeP
  pure $ SchemeLambda $ Lambda params defs body
  where
    syn = do
      ws
      v <- some (charP isLetter)
      ws
      pure v


funcNameP :: Parser String
funcNameP = do
  beginning <- some noNum
  ending    <- many withNum
  let funcName = beginning ++ ending
  if notKeyword funcName
  then pure funcName
  else empty
  where
    noNum = asum
      [ charP (=='*')
      , charP (=='+')
      , charP (=='-')
      , charP (=='?')
      , charP (=='\'')
      , charP isLetter ]
    withNum = noNum <|> charP isDigit

funCallP :: Parser SchemeValue
funCallP = bracket do
  func <- funcNameP
  args <- many schemeP
  pure $ SchemeFunctionCall func args

lambdaCallP :: Parser SchemeValue
lambdaCallP = bracket do
  v <- schemeP
  args <- many schemeP
  pure $ SchemeLambdaCall v args

commentsP :: Parser [()]
commentsP = many $ do
  ws
  _ <- charP (==';')
  _ <- many $ charP (/='\n')
  pure ()

schemeP ::Parser SchemeValue
schemeP = commentsP *> ws *> asum
  [ boolP
  , funCallP
  , lambdaCallP
  , doubleP
  , integerP
  , quoteP
  , schemeStringP
  , SchemeVariable <$> varP
  , condP
  , ifP
  , SchemeDefinition <$> defP
  , lambdaP
  ] <* ws



--main :: IO ()
--main = undefined
