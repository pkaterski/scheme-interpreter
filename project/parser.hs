{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

import Control.Applicative
import Data.Char

-- ideas: else == #t as a keyword and synonym
-- TODO maybe add separate types for complex ones
data SchemeValue
  = SchemeBool Bool
  | SchemeInteger Integer
  | SchemeDouble Double
  | SchemeString String
  | SchemeSymbol String -- for lists
  | SchemeList [SchemeValue]
  | SchemeSynonym String -- any non-keyword
  | SchemeIf (SchemeValue,SchemeValue,SchemeValue)
  | SchemeCond ([(SchemeValue,SchemeValue)])
  | SchemeDefinition (String,[String],SchemeValue)
  | SchemeLambda ([String], SchemeValue)
  | SchemeFunctionCall (String, [SchemeValue])
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
  empty = Parser \_ -> Nothing 
  Parser p1 <|> Parser p2 = Parser \inp ->
    case p1 inp of
      Nothing -> p2 inp
      out     -> out 

instance Monad Parser where
  Parser px >>= f = Parser \inp -> do
    (x, inp') <- px inp
    runParser (f x) inp' 
    

charP :: (Char -> Bool) -> Parser Char
charP p = Parser \inp -> case inp of
  (x:xs) | p x -> Just (x, xs)
  _            -> Nothing 
  
stringP :: String -> Parser String
stringP = traverse (\x -> charP (==x)) 

boolP :: Parser SchemeValue
boolP = trueP <|> falseP
  where
    trueP  = SchemeBool True  <$ stringP "#t"
    falseP = SchemeBool False <$ stringP "#f"


-- this could be refactored.. to Parser Integer and simplified below
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
  return (SchemeInteger $ read ds) 


doubleP :: Parser SchemeValue
doubleP = do
  n <- negDigitsP <|> digitsP
  _ <- charP (=='.')
  m <- digitsP 
  return $ SchemeDouble (read $ n++'.':m)


-- no escape support
schemeStringP :: Parser SchemeValue
schemeStringP = do
  charP (=='"')
  x <- many $ charP (/='"')
  charP (=='"')
  return $ SchemeString x 

ws :: Parser String 
ws = many $ charP isSpace 

unbracket :: Parser a -> Parser a
unbracket p = do
  ws *> charP (=='(') *> ws
  x <- p
  ws *> charP (==')') *> ws
  return x

isWord :: String -> Parser String
isWord s = ws *> stringP s 

ifP :: Parser SchemeValue
ifP = do
  x <- unbracket $ (,,)
    <$> (isWord "if" *> schemeP)
    <*> schemeP
    <*> schemeP
  return $ SchemeIf x

forbidden :: [String]
forbidden = ["if", "cond", "define", "lambda"]

notKeyword :: String -> Bool
notKeyword x = not $ x `elem` forbidden 

synonymP :: Parser SchemeValue
synonymP = do
  xs <- some $ charP isLetter
  ys <- many $ charP (liftA2 (||) isLetter isDigit)
  let s = (xs++ys) in
    if notKeyword s 
    then return $ SchemeSynonym s 
    else empty 

condP :: Parser SchemeValue
condP = unbracket do
    isWord "cond"
    vs <- some pair
    return $ SchemeCond vs
  where 
    pair = unbracket $ (,)
      <$> schemeP
      <*> schemeP

-- is this usefull. scheme supports it but idk 
symbolP :: Parser SchemeValue
symbolP = do 
  xs <- some $ charP isLetter
  ys <- many $ charP (liftA2 (||) isLetter isDigit)
  return $ SchemeSymbol (xs++ys) 

listP :: Parser SchemeValue
listP = do
    charP (=='\'')
    vs <- unbracket $ many vals 
    return $ SchemeList vs
  where 
    purevals = 
          boolP 
      <|> integerP 
      <|> doubleP 
      <|> symbolP 
      <|> fmap SchemeList (unbracket $ many vals) 
    vals = ws *> purevals <* ws

-- TODO do notation below maybe?
defP :: Parser SchemeValue
defP = SchemeDefinition
    <$> unbracket 
        do isWord "define" *> (comb <$> head <*> body)
  where 
    comb (x,ys) zs = (x,ys,zs)
    head =  (,[]) <$> syn 
        <|> unbracket ((,) <$> syn <*> (many syn)) 
    syn = ws *> (some $ charP isLetter) <* ws
    body = schemeP <|> unbracket schemeP


lambdaP :: Parser SchemeValue
lambdaP = fmap SchemeLambda
   $ unbracket (isWord "lambda" *> 
                  ((,) <$> head <*> body))
  where 
    head = (:[]) <$> syn 
        <|> unbracket (many syn) 
    syn = ws *> (some $ charP isLetter) <* ws
    body = schemeP <|> unbracket schemeP


funCallP :: Parser SchemeValue
funCallP = fmap SchemeFunctionCall 
         $ unbracket ((,) <$> fun <*> (many schemeP))
  where 
    fun = do
      ws <- nonSpaceP
      if notKeyword ws
      then return ws
      else empty
    nonSpaceP = ws *> (some $ charP (/=' ')) <* ws

schemeP ::Parser SchemeValue
schemeP = ws *> (
    boolP 
  <|> doubleP 
  <|> integerP
  <|> schemeStringP
  <|> synonymP
  <|> condP 
  <|> listP 
  <|> ifP 
  <|> defP 
  <|> lambdaP 
  <|> funCallP 
  ) <* ws 



main :: IO ()
main = undefined
