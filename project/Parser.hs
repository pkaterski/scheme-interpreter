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
  | SchemeSymbol String -- for lists
  | SchemeList [SchemeValue]
  | SchemeSynonym String -- any non-keyword
  | SchemeIf SchemeValue SchemeValue SchemeValue
  | SchemeCond [(SchemeValue,SchemeValue)]
  | SchemeDefinition Definition 
  | SchemeLambda Lambda 
  | SchemeFunctionCall String [SchemeValue]
  deriving (Eq, Show)

data Definition = Definition String Lambda 
  deriving (Eq, Show)

data Lambda = Lambda [String] [Definition] SchemeValue
  deriving (Eq, Show)


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

synonymP :: Parser String
synonymP = do
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

-- is this usefull. scheme supports it but idk
symbolP :: Parser SchemeValue
symbolP = do
  xs <- some $ charP isLetter
  ys <- many $ charP $ liftA2 (||) isLetter isDigit
  return $ SchemeSymbol (xs++ys)

listP :: Parser SchemeValue
listP = do
    charP (=='\'')
    vs <- bracket $ many vals
    return $ SchemeList vs
  where
    purevals =
          boolP
      <|> doubleP
      <|> integerP
      <|> symbolP
      <|> fmap SchemeList do bracket $ many vals
    vals = ws *> purevals <* ws



defP :: Parser Definition 
defP = bracket do
  isWord "define"
  ws
  let idents = Right <$> bracket (some synonymP)
      singleIdent = Left <$> synonymP

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
  , doubleP
  , integerP
  , schemeStringP
  , SchemeSynonym <$> synonymP
  , condP
  , listP
  , ifP
  , SchemeDefinition <$> defP
  , lambdaP
  ] <* ws



--main :: IO ()
--main = undefined
