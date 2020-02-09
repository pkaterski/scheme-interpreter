import Parser 
import Control.Applicative 
import Data.Foldable (asum)

type State = ([SchemeValue], [SchemeValue])

getDefinitions :: State -> [SchemeValue]
getDefinitions = fst 

getUninterpreted :: State -> [SchemeValue]
getUninterpreted = snd


newtype Eval a = S (State -> Either (a,State) String)

app :: Eval a -> State -> Either (a,State) String
app (S st) x = st x

instance Functor Eval where
  fmap g sx = do
    x <- sx
    pure $ g x 

instance Applicative Eval where
  pure x = S $ \s -> Left (x, s)
  sf <*> sx = do
    f <- sf
    x <- sx
    pure $ f x 
         
instance Monad Eval where
  sx >>= f = S $ \s -> 
    let ms = app sx s 
    in case ms of
      Left (x, s') -> app (f x) s'
      Right s      -> Right s 

instance Alternative Eval where
  empty = S $ \_ -> Right "no scheme value" 
  S sx <|> S sy = S $ \s ->
    case sx s of
      Right _ -> sy s
      out     -> out 


findDefinition :: String -> [SchemeValue] -> Maybe SchemeValue
findDefinition s (d@(SchemeDefinition s' args body):ds) = 
  if s == s' 
  then case args of
    [] -> Just body
    _  -> Just d -- TODO lambda ? 
  else findDefinition s ds
findDefinition s (_:ds) = findDefinition s ds
findDefinition s [] = Nothing 

evalBool :: Eval SchemeValue 
evalBool = S eval
  where
    eval (ds, (r@(SchemeBool _):xs)) = Left (r,(ds,xs))
    eval _ = Right "no bool" 

evalString :: Eval SchemeValue 
evalString = S eval
  where
    eval (ds, (r@(SchemeString _):xs)) = Left (r,(ds,xs))
    eval _ = Right "no string" 


evalInteger :: Eval SchemeValue 
evalInteger = S eval
  where
    eval (ds, (r@(SchemeInteger _):xs)) = Left (r,(ds,xs))
    eval _ = Right "no int" 


evalDouble :: Eval SchemeValue 
evalDouble = S eval
  where
    eval (ds, (r@(SchemeDouble _):xs)) = Left (r,(ds,xs))
    eval _ = Right "no double" 


evalList :: Eval SchemeValue 
evalList = S eval
  where
    eval (ds, (r@(SchemeList _):xs)) = Left (r,(ds,xs))
    eval _ = Right "no list" 

evalIf :: Eval SchemeValue
evalIf = do
    (p, t, f) <- S eval
    p' <- evalVal p
    case p' of
      SchemeBool True -> pure t
      SchemeBool False -> pure f
      _ -> empty
  where
    eval (ds, ((SchemeIf p t f):xs)) = Left ((p,t,f), (ds,xs)) 
    eval _ = Right "no if" 

evalVal :: SchemeValue -> Eval SchemeValue
evalVal v = S $ \(ds,_) ->
   app evalScheme (ds,[v])

evalSynonym :: Eval SchemeValue
evalSynonym = S eval 
  where
    eval (ds, ((SchemeSynonym r):xs)) = 
      case findDefinition r ds of
        Just x  -> Left (x,(ds,xs))
        Nothing -> Right ("Variable " ++ r ++ " isnt defined")
    eval _ = Right "no synonym"

evalCond :: Eval SchemeValue
evalCond = do
    psts <- S eval
    findTrue psts 
  where
    eval (ds, ((SchemeCond psts):xs)) = Left (psts, (ds,xs)) 
    eval _ = Right "no cond" 
    findTrue ((p,t):psts) = do
      p' <- evalVal p
      case p' of
        SchemeBool True -> pure t
        SchemeBool False -> findTrue psts
        _ -> S $ \_ -> Right "not a bool in cond case"
    findTrue [] = S $ \_ -> Right "no default case"

evalScheme :: Eval SchemeValue
evalScheme = asum
  [ evalBool
  , evalString
  , evalInteger
  , evalDouble
  , evalIf
  , evalSynonym
  , evalList
  , evalCond
  ]

defaultDefs = 
  [ SchemeDefinition "else" [] (SchemeBool True)
  ]

main :: IO ()
main = do 
  x <- readFile "test/example.scm" 
  putStrLn $ 
    case many schemeP `runParser` x of
      Just s -> show s
      Nothing -> "kur"
  return ()



