import Control.Applicative
import Data.Char

-- ideas: else == #t as a keyword and synonym
-- TODO add lambda
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
  | SchemeFunctionCall (String, [SchemeValue])
  deriving (Eq, Show)


newtype Parser a = Parser {runParser :: String -> Maybe (a,String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> do
    (x, inp') <- p inp
    return (f x, inp') 

instance Applicative Parser where
  pure x = Parser $ \inp -> Just (x, inp) 
  Parser f <*> Parser p = Parser $ \inp -> do
    (f, inp') <- f inp 
    (x, inp'')  <- p inp'
    return (f x, inp'') 

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing 
  Parser p1 <|> Parser p2 = Parser $ \inp ->
    case p1 inp of
      Nothing -> p2 inp
      out     -> out 

charP :: Char -> Parser Char
charP c = Parser $ \inp -> case inp of
  (x:xs) | x == c -> Just (x, xs)
  _               -> Nothing 
  
stringP :: String -> Parser String
stringP = sequenceA . map charP 

-- TODO: a better way: (trueP <|> falseP)...
boolP :: Parser SchemeValue
boolP = fmap (\b -> SchemeBool $ case b of
  "#t" -> True
  "#f" -> False
  _    -> undefined) -- this should never happen :)
  $ (stringP "#t" <|> stringP "#f") 


spanP :: (Char -> Bool) -> Parser String
spanP p = notNull (Parser $ \inp -> Just $ span p inp)

-- TODO remvoe unsuded notNull in safe one
spanUnsafeP :: (Char -> Bool) -> Parser String
spanUnsafeP p = Parser $ \inp -> Just $ span p inp

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \inp -> do
  (xs, inp') <- p inp
  if null xs then
    Nothing
  else return (xs, inp') 

-- this could be refactored.. to Parser Integer and simplified below
digitsP :: Parser String
digitsP = notNull $ spanP isDigit 

integerP :: Parser SchemeValue 
integerP = 
  f 
  <$> notNull (((:) <$> charP '-' <*> digitsP) 
  <|> digitsP)
  where f ds = SchemeInteger $ read ds

-- this is utter boza
doubleP :: Parser SchemeValue
doubleP 
   =  fmap (\x -> SchemeDouble $ read x) 
   $  comb 
  <$> (((:) <$> charP '-' <*> digitsP) <|> digitsP) 
  <*> charP '.' 
  <*> digitsP 
    where comb xs x ys = xs++x:ys


-- no escape support
schemeStringP :: Parser SchemeValue
schemeStringP = fmap SchemeString $ charP '"' *> 
                  spanUnsafeP (/='"') 
                <* charP '"'
    

ws :: Parser String 
ws = spanUnsafeP isSpace 

unbracket :: Parser a -> Parser a
unbracket p = ws *> charP '(' *> ws *> p <* ws <* charP ')' <* ws

isWord :: String -> Parser String
isWord s = ws *> stringP s 

ifP :: Parser SchemeValue
ifP = 
  fmap SchemeIf $
  unbracket ((,,) <$> (isWord "if" *> schemeP) <*> schemeP <*> schemeP)

forbidden :: [String]
forbidden = ["if", "cond", "define"]

notKeyword :: String -> Bool
notKeyword x = not $ x `elem` forbidden 

-- TODO add support for letters followed by numbers
synonymP :: Parser SchemeValue
synonymP = Parser $ \inp -> do
  (s,inp') <- runParser (notNull $ spanP isLetter) inp
  if notKeyword s then
    return (SchemeSynonym s,inp')
  else
    Nothing

condP :: Parser SchemeValue
condP = fmap SchemeCond 
  $ unbracket 
  (isWord "cond" *> some (unbracket ((,) <$> schemeP <*> schemeP)))

-- is it needed??
-- if so also: TODO support for numbers after letters 
symbolP :: Parser SchemeValue
symbolP = fmap SchemeSynonym $ spanP isLetter

listP :: Parser SchemeValue
listP = fmap SchemeList 
  $  charP '\'' 
  *> unbracket (many vals)
    where purevals = 
                 boolP 
             <|> integerP 
             <|> doubleP 
             <|> synonymP -- is this going to break something ??
             <|> fmap SchemeList (unbracket $ many vals) --lists inside lists
          vals = ws *> purevals <* ws

-- unbeleavable boza
defP :: Parser SchemeValue
defP = fmap SchemeDefinition
   $ unbracket (isWord "define" *> 
                  (comb <$> head <*> body))
  where 
    comb (x,ys) zs = (x,ys,zs)
    head =  (fmap (\x -> (x,[])) syn) 
        <|> unbracket ((,) <$> syn <*> (many syn)) 
    syn = ws *> spanP isLetter <* ws
    body = schemeP

funCallP :: Parser SchemeValue
funCallP = fmap SchemeFunctionCall 
         $ unbracket ((,) <$> fun <*> (many schemeP))
  where 
    fun = Parser $ \inp -> do
      (ws, inp') <- runParser (ws *> spanP (/=' ') <* ws) inp
      if notKeyword ws then
        return (ws, inp')
      else Nothing

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
  <|> funCallP 
  ) <* ws 



main :: IO ()
main = undefined
