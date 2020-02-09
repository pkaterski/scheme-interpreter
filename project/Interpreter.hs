import Parser 
import Control.Applicative 
import Data.Foldable (asum)

type State = ([SchemeValue], [SchemeValue])

getDefinitions :: State -> [SchemeValue]
getDefinitions = fst 

getUninterpreted :: State -> [SchemeValue]
getUninterpreted = snd


newtype Eval a = S (State -> Either String (a,State))

app :: Eval a -> State -> Either String (a,State)
app (S st) x = st x

instance Functor Eval where
  fmap g sx = do
    x <- sx
    pure $ g x 

instance Applicative Eval where
  pure x = S $ \s -> Right (x, s)
  sf <*> sx = do
    f <- sf
    x <- sx
    pure $ f x 
         
instance Monad Eval where
  sx >>= f = S $ \s -> 
    let ms = app sx s 
    in case ms of
      Right (x, s') -> app (f x) s'
      Left s        -> Left s 

instance Alternative Eval where
  empty = S $ \_ -> Left "no scheme value" 
  S sx <|> S sy = S $ \s ->
    case sx s of
      Left _ -> sy s
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
    eval (ds, (r@(SchemeBool _):xs)) = Right (r,(ds,xs))
    eval _ = Left "no bool" 

evalString :: Eval SchemeValue 
evalString = S eval
  where
    eval (ds, (r@(SchemeString _):xs)) = Right (r,(ds,xs))
    eval _ = Left "no string" 


evalInteger :: Eval SchemeValue 
evalInteger = S eval
  where
    eval (ds, (r@(SchemeInteger _):xs)) = Right (r,(ds,xs))
    eval _ = Left "no int" 


evalDouble :: Eval SchemeValue 
evalDouble = S eval
  where
    eval (ds, (r@(SchemeDouble _):xs)) = Right (r,(ds,xs))
    eval _ = Left "no double" 


evalList :: Eval SchemeValue 
evalList = S eval
  where
    eval (ds, (r@(SchemeList _):xs)) = Right (r,(ds,xs))
    eval _ = Left "no list" 

evalIf :: Eval SchemeValue
evalIf = do
    (p, t, f) <- S eval
    p' <- evalVal p
    case p' of
      SchemeBool True -> pure t
      SchemeBool False -> pure f
      _ -> empty
  where
    eval (ds, ((SchemeIf p t f):xs)) = Right ((p,t,f), (ds,xs)) 
    eval _ = Left "no if" 

evalVal :: SchemeValue -> Eval SchemeValue
evalVal v = S $ \(ds,_) ->
   app evalScheme (ds,[v])

evalSynonym :: Eval SchemeValue
evalSynonym = S eval 
  where
    eval (ds, ((SchemeSynonym r):xs)) = 
      case findDefinition r ds of
        Just x  -> Right (x,(ds,xs))
        Nothing -> Left ("Variable " ++ r ++ " isnt defined")
    eval _ = Left "no synonym"

evalCond :: Eval SchemeValue
evalCond = do
    psts <- S eval
    findTrue psts 
  where
    eval (ds, ((SchemeCond psts):xs)) = Right (psts, (ds,xs)) 
    eval _ = Left "no cond" 
    findTrue ((p,t):psts) = do
      p' <- evalVal p
      case p' of
        SchemeBool True -> pure t
        SchemeBool False -> findTrue psts
        _ -> S $ \_ -> Left "not a bool in cond case"
    findTrue [] = S $ \_ -> Left "no default case"

evalDefinition :: Eval SchemeValue
evalDefinition = S eval
  where 
    eval (ds, (d@(SchemeDefinition r _ _):xs)) = 
      case findDefinition r ds of
        Nothing -> Right (d, (d:ds,xs)) 
        _       -> Left "that name is already defined"
    eval _ = Left "no definition" 


evalFunCall :: Eval SchemeValue
evalFunCall =
  S eval 
  where
    eval (ds, ((SchemeFunctionCall f args):xs)) = 
      case f of
        "-" -> undefined
        "+" -> undefined
        "car" -> undefined
        "cdr" -> undefined
        _ -> case findDefinition f ds of
          Just (SchemeDefinition g params body) ->
            let synonyms = match ds args params
            in case synonyms of
              -- putting ss infront should redifine
              Right ss -> app (evalVal body) (ss++ds,[]) 
              Left t -> Left t

          _ -> Left "no such function defined"
    eval _ = Left "no function call" 

    match :: [SchemeValue] -> [SchemeValue] -> [String] -> Either String [SchemeValue]
    match ds (v:vs) (r:rs) = 
      case app (evalVal v) (ds,[]) of
        Right (v',_) -> do
          ms <- match ds vs rs
          pure $ SchemeDefinition r [] v' : ms 
        Left t -> Left t 
    match _ [] [] = Right []
    match _ [] r = Left "args are not enough"
    match _ _ [] = Left "params are not enough" 

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
  , evalDefinition
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



