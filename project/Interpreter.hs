import Parser 
import Control.Applicative 
import Data.Foldable (asum)

type State = ([SchemeValue], [SchemeValue])

getDefinitions :: State -> [SchemeValue]
getDefinitions = fst 

getUninterpreted :: State -> [SchemeValue]
getUninterpreted = snd


newtype Eval a = S (State -> Maybe (a,State))

app :: Eval a -> State -> Maybe (a,State)
app (S st) x = st x

instance Functor Eval where
  fmap g sx = do
    x <- sx
    pure $ g x 

instance Applicative Eval where
  pure x = S $ \s -> Just (x, s)
  sf <*> sx = do
    f <- sf
    x <- sx
    pure $ f x 
         
instance Monad Eval where
  sx >>= f = S $ \s -> 
    let ms = app sx s 
    in case ms of
      Just (x, s') -> app (f x) s'
      Nothing      -> Nothing 

instance Alternative Eval where
  empty = S $ \_ -> Nothing
  S sx <|> S sy = S $ \s ->
    case sx s of
      Nothing -> sy s
      out     -> out 


findDefinition :: String -> [SchemeValue] -> Maybe SchemeValue
findDefinition s (d@(SchemeDefinition s' _ _):ds) = 
  if s == s' 
  then Just d 
  else findDefinition s ds
findDefinition s (_:ds) = findDefinition s ds
findDefinition s [] = Nothing 

evalBool :: Eval SchemeValue 
evalBool = S eval
  where
    eval (ds, (r@(SchemeBool _):xs)) = Just (r,(ds,xs))
    eval _ = Nothing

evalString :: Eval SchemeValue 
evalString = S eval
  where
    eval (ds, (r@(SchemeString _):xs)) = Just (r,(ds,xs))
    eval _ = Nothing


evalInteger :: Eval SchemeValue 
evalInteger = S eval
  where
    eval (ds, (r@(SchemeInteger _):xs)) = Just (r,(ds,xs))
    eval _ = Nothing


evalDouble :: Eval SchemeValue 
evalDouble = S eval
  where
    eval (ds, (r@(SchemeDouble _):xs)) = Just (r,(ds,xs))
    eval _ = Nothing

evalId :: Eval SchemeValue
evalId = S $ \s@(ds, (x:xs)) -> Just (x, (ds, xs))

evalIf :: Eval SchemeValue
evalIf = do
    (p, t, f) <- S eval
    p' <- evalVal p
    case p' of
      SchemeBool True -> pure t
      SchemeBool False -> pure f
      _ -> empty
  where
    eval (ds, ((SchemeIf p t f):xs)) = Just ((p,t,f), (ds,xs)) 
    eval _ = Nothing

evalVal :: SchemeValue -> Eval SchemeValue
evalVal v = S $ \(ds,_) ->
   app evalScheme (ds,[v])


evalScheme :: Eval SchemeValue
evalScheme = asum
  [ evalBool
  , evalString
  , evalInteger
  , evalDouble
  , evalIf
  ]

main :: IO ()
main = do 
  x <- readFile "test/example.scm" 
  putStrLn $ 
    case many schemeP `runParser` x of
      Just s -> show s
      Nothing -> "kur"
  return ()



