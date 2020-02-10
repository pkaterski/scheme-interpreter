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
      SchemeBool True -> evalVal t
      SchemeBool False -> evalVal f
      _ -> empty
  where
    eval (ds, ((SchemeIf p t f):xs)) = Right ((p,t,f), (ds,xs)) 
    eval _ = Left "no if" 

evalVal :: SchemeValue -> Eval SchemeValue
evalVal v = S $ \(ds,xs) ->
   app evalScheme (ds,(v:xs))

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
        SchemeBool True -> evalVal t
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
        "-" -> undefined --not supported yet
        "+" -> do
          s <- evalPlus ds args
          Right (s,(ds,xs)) 
        "*" -> do
          s <- evalProd ds args
          Right (s,(ds,xs)) 
        "car" -> do
          s <- evalCar ds args
          Right (s,(ds,xs)) 
        "cdr" -> do
          s <- evalCdr ds args
          Right (s,(ds,xs)) 
        "cons" -> do
          s <- evalCons ds args
          Right (s,(ds,xs)) 
        "eq?" -> do
          s <- evalEq ds args
          Right (s,(ds,xs)) 
        _ -> case findDefinition f ds of
          Just (SchemeDefinition g params body) ->
            let synonyms = match ds args params
            in case synonyms of
              Right ss -> do
                -- putting ss infront should redefine
                (v',_) <- app (evalVal body) (ss++ds,[]) 
                pure (v',(ds,xs))
              Left t -> Left t

          Nothing -> Left "no such function defined"
    eval _ = Left "no function call" 

    match :: [SchemeValue] -> [SchemeValue] -> [String] -> Either String [SchemeValue]
    match ds (v:vs) (r:rs) = 
      case app (evalVal v) (ds,[]) of
        Right (v',_) -> do
          ms <- match ds vs rs
          pure $ SchemeDefinition r [] v' : ms 
        Left t -> Left t 
    match _ [] [] = Right []
    match _ [] _ = Left "args are not enough"
    match _ _ [] = Left "params are not enough" 

evalPlus :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalPlus ds (x:xs) = do
  (v,_) <- app (evalVal x) (ds,[])
  case v of
    SchemeInteger x -> do
      s <- evalPlus ds xs
      case s of
        SchemeDouble s -> 
          pure $ SchemeDouble $ (fromIntegral x) + s
        _ -> error "imposible"
    SchemeDouble x -> do
      s <- evalPlus ds xs
      case s of
        SchemeDouble s -> 
          pure $ SchemeDouble $ x + s
        _ -> error "imposible"
    _ -> Left "cant add non-numbers"
evalPlus _ [] = Right $ SchemeDouble 0



evalProd :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalProd ds (x:xs) = do
  (v,_) <- app (evalVal x) (ds,[])
  case v of
    SchemeInteger x -> do
      s <- evalProd ds xs
      case s of
        SchemeDouble s -> 
          pure $ SchemeDouble $ (fromIntegral x) * s
        _ -> error "imposible"
    SchemeDouble x -> do
      s <- evalProd ds xs
      case s of
        SchemeDouble s -> 
          pure $ SchemeDouble $ x * s
        _ -> error "imposible"
    _ -> Left "cant add non-numbers"
evalProd _ [] = Right $ SchemeDouble 1


evalEq :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalEq ds (x:y:[]) = do
  (s,_) <- app (evalVal x) (ds,[]) 
  (t,_) <- app (evalVal y) (ds,[]) 
  pure $ SchemeBool $ s == t
evalEq _ _ = Left "you can only compare 2 values"

evalCar :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalCar ds (l:[]) = do
  (l',_) <- app (evalVal l) (ds,[])
  case l' of
    SchemeList xs -> pure $ head xs 
    _ -> Left "car accepts only lists"
evalCar _ _ = Left "car accepts 1 arg"


evalCons :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalCons ds (v:xs:[]) = do
  (v',_)  <- app (evalVal v) (ds,[])
  (xs',_) <- app (evalVal xs) (ds,[])
  case xs' of
    SchemeList ys -> Right $ SchemeList (v':ys)
    _             -> Left "cant cons to non-list"
  
evalCons _ _ = Left "cons takes 2 arguments"


evalCdr :: [SchemeValue] -> [SchemeValue] -> Either String SchemeValue
evalCdr ds (l:[]) = do
  (l',_) <- app (evalVal l) (ds,[])
  case l' of
    SchemeList xs -> pure $ SchemeList $ tail xs 
    _ -> Left "cdr accepts only lists"
evalCdr _ _ = Left "cdr accepts 1 arg"


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
  , evalFunCall
  ]

defaultDefs = 
  [ SchemeDefinition "else" [] (SchemeBool True)
  ]

main :: IO ()
main = do 
  x <- readFile "test/example.scm" 
  putStrLn $ 
    case many schemeP `runParser` x of
      Just (s,_) ->
        case app shitEval (defaultDefs, s) of
          Right (rs,_) -> disp rs
          Left t -> t
      Nothing -> "didn't parse"
  return ()

-- this is very userful for debugging this shit
shitEval = many evalScheme

disp :: [SchemeValue] -> String
disp (SchemeDefinition _ _ _:xs) = disp xs 
disp (x:xs) = show x ++ "\n" ++ disp xs
disp [] = ""



