{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE LambdaCase #-}

import Parser
import Control.Applicative

type State = [Definition]

newtype Eval a = Eval {runEval :: State -> Either String (State, a)}

instance Functor Eval where
  fmap g ev = Eval \s -> case runEval ev s of
    Right (s', x) -> Right (s', g x)
    Left err      -> Left err

instance Applicative Eval where
  pure x = Eval \s -> Right (s, x)
  fx <*> sx = Eval \s -> do
    (s', f)  <- runEval fx s
    (s'', x) <- runEval sx s'
    pure (s'', f x)

instance Monad Eval where
  sx >>= f = Eval \s -> do
    (s', x)  <- runEval sx s
    (s'', y) <- runEval (f x) s'
    pure (s'', y)

get :: Eval State
get = Eval \s -> Right (s, s)

put :: State -> Eval ()
put s = Eval \_ -> Right (s, ())

oops :: String -> Eval a
oops err = Eval \s -> Left $ err -- ++ "\n the env is:\n" ++ show s

searchDefinition :: String -> [Definition] -> Maybe SchemeValue
searchDefinition _ [] = Nothing
searchDefinition s (Definition s' l@(Lambda args _ body):ds) =
  if s == s'
  then case args of
    [] -> Just body -- lambda w/ no args is a val
    _  -> Just $ SchemeLambda l
  else searchDefinition s ds

isDefined :: String -> [Definition] -> Bool
isDefined s ds = case searchDefinition s ds of
  Just _  -> True
  Nothing -> False


evalIf :: SchemeValue -> Eval SchemeValue
evalIf (SchemeIf p t f) = do
  p' <- eval p
  case p' of
    SchemeBool True  -> eval t
    SchemeBool False -> eval f
    _                -> oops $ "condition in if not a bool: " ++ show p

evalCond :: SchemeValue -> Eval SchemeValue
evalCond (SchemeCond ((p,v):xs)) = do
  p' <- eval p
  case p' of
    SchemeBool True  -> eval v
    SchemeBool False -> evalCond $ SchemeCond xs
    _                -> oops $ "condition in cond not a bool: " ++ show p


evalDefinition :: SchemeValue -> Eval SchemeValue
evalDefinition (SchemeDefinition v@(Definition s _)) = do
  currDefs <- get
  if isDefined s currDefs
  then oops $ "already existing def: " ++ show v
  else
    do put (v:currDefs)
       pure $ SchemeDefinition v

evalSynonym :: SchemeValue -> Eval SchemeValue
evalSynonym v@(SchemeVariable s) = do
  currDefs <- get
  case searchDefinition s currDefs of
    Just d -> eval d
    Nothing -> oops $ "unknow variable used: " ++ show v


evalFunctionCall :: SchemeValue -> Eval SchemeValue
evalFunctionCall v@(SchemeFunctionCall s args) =
  if isBuildin v
  then evalBuildin v
  else do
  currDefs <- get
  case searchDefinition s currDefs of
    Just (SchemeLambda (Lambda params ldef body)) -> do
      ds <- match args params
      case runEval (evalRec $ body:[]) (ds++ldef++currDefs) of
       Right (_,res) -> pure $ last res
       Left s        -> oops $ "local eval err: " ++ s
    Just s -> oops $ "unexpected value in definition: " ++ show s
    Nothing -> oops $ "unknow function is called: " ++ show v


evalLambdaCall :: SchemeValue -> Eval SchemeValue
evalLambdaCall (SchemeLambdaCall l args) = do
  l' <- eval l
  case l' of
    (SchemeLambda (Lambda params ldef body)) -> do
      currDefs <- get
      ds <- match args params
      case runEval (evalRec $ body:[]) (ds++ldef++currDefs) of
       Right (_,res) -> pure $ last res
       Left s        -> oops $ "local eval err: " ++ s

    _ -> oops $ "trying to call a non-function: " ++ show l



match :: [SchemeValue] -> [String] -> Eval [Definition]
-- match args params
match (a:as) (p:ps) = do
  
  a' <- case a of
    SchemeDefinition (Definition _ l) ->
      pure $ Definition p l
    _ -> do
      a' <- eval a
      pure $ Definition p (Lambda [] [] a')

  xs <- match as ps
  pure $ a' : xs
match [] [] = pure []
match [] ps  = oops $ "match: args are not enough, unused params: " ++ show ps
match as []  = oops $ "match: params are not enough, unused args: " ++ show as


isBuildin :: SchemeValue -> Bool
isBuildin (SchemeFunctionCall s _) = s `elem` ["+", "*", "car", "cdr", "cons", "eq?", "eval"]


evalBuildin :: SchemeValue -> Eval SchemeValue
evalBuildin (SchemeFunctionCall "+" xs) = evalBuildinPlus xs
evalBuildin (SchemeFunctionCall "*" xs) = evalBuildinProd xs
evalBuildin (SchemeFunctionCall "car" xs) = evalBuildinCar xs
evalBuildin (SchemeFunctionCall "cdr" xs) = evalBuildinCdr xs
evalBuildin (SchemeFunctionCall "cons" xs) = evalBuildinCons xs
evalBuildin (SchemeFunctionCall "eq?" xs) = evalBuildinEq xs
evalBuildin (SchemeFunctionCall "eval" xs) = evalBuildinEval xs


evalBuildinEval :: [SchemeValue] -> Eval SchemeValue
evalBuildinEval (x:[]) = do
  x' <- eval x
  case x' of
    SchemeQuote xs -> do
      case runParser schemeP xs of
        Just (v,_) -> eval v
        _          -> oops $ "eval can't parse: " ++ xs
    _              -> oops $ "eval only works with quotes: " ++ show x'
evalBuildinEval xs  = oops $ "eval too many args: " ++ show xs


evalBuildinPlus :: [SchemeValue] -> Eval SchemeValue
evalBuildinPlus (x:xs) = do
  x' <- eval x
  xs' <- evalBuildinPlus xs
  case x' of
    SchemeInteger i -> case xs' of
      SchemeInteger j -> pure $ SchemeInteger $ i + j
      SchemeDouble j -> pure $ SchemeDouble $ fromIntegral i + j
    SchemeDouble i -> case xs' of
      SchemeDouble j -> pure $ SchemeDouble $ i + j
      SchemeInteger j -> pure $ SchemeDouble $ i + fromIntegral j

    _ -> oops $ "cannot sum non-number: " ++ show x
evalBuildinPlus [] = pure $ SchemeInteger 0

evalBuildinProd :: [SchemeValue] -> Eval SchemeValue
evalBuildinProd (x:xs) = do
  x' <- eval x
  xs' <- evalBuildinProd xs
  case x' of
    SchemeInteger i -> case xs' of
      SchemeInteger j -> pure $ SchemeInteger $ i * j
      SchemeDouble j -> pure $ SchemeDouble $ fromIntegral i * j
    SchemeDouble i -> case xs' of
      SchemeDouble j -> pure $ SchemeDouble $ i * j
      SchemeInteger j -> pure $ SchemeDouble $ i * fromIntegral j

    _ -> oops $ "cannot sum non-number: " ++ show x
evalBuildinProd [] = pure $ SchemeInteger 1

evalBuildinCar :: [SchemeValue] -> Eval SchemeValue
evalBuildinCar (v : []) = do
  v' <- eval v
  case v' of
    SchemeQuote xs -> do
      case runParser inside xs of
        Just (s, _)  -> do
          case runParser purevals s of
            Just (x, []) -> pure x
            _            -> pure $ SchemeSymbol s
        Nothing -> oops $ "cannot car `unparsable`:" ++ xs
    _ -> oops $ "car arg not quote: " ++ show v'
  where
    inside = bracket do
      x <-  some $ charP $ liftA2 (&&) (/=' ') (/=')')
      many $ charP (/=')')
      pure x

    purevals =
          boolP
      <|> doubleP
      <|> integerP
      <|> symbolP
evalBuildinCar xs = oops $ "car different than one args: " ++ show xs


evalBuildinCdr :: [SchemeValue] -> Eval SchemeValue
evalBuildinCdr (v : []) = do
  v' <- eval v
  case v' of
    SchemeQuote xs ->
      case runParser inside xs of
        Just (s, _)  -> pure $ SchemeQuote s
        Nothing -> oops $ "cannot car `unparsable`:" ++ xs
    _ -> oops $ "cdr not quote: " ++ show v'
  where
    inside = bracket do
      some $ charP $ liftA2 (&&) (/=' ') (/=')')
      ws
      x <- many $ charP (/=')')
      pure $ '(' : x ++ ")"
evalBuildinCdr xs = oops $ "cdr different than one args: " ++ show xs


evalBuildinCons :: [SchemeValue] -> Eval SchemeValue
evalBuildinCons (x : v : []) = do
  x' <- eval x
  v' <- eval v
  case v' of
    SchemeQuote s -> 

      case x' of
        SchemeInteger i -> insert (show i) s
        SchemeDouble i  -> insert (show i) s
        SchemeString i  -> insert i s
        SchemeSymbol i  -> insert i s
        SchemeQuote i   -> insert i s
        x               -> oops $ "cons to unsupported type: " ++ show x

    _ -> oops $ "cons only works with quotes (pair not supported yet): " ++ show x
  where
    insert first ('(':")") = pure $ SchemeQuote $ "(" ++ first  ++ ")"
    insert first ('(':xs)  = pure $ SchemeQuote $ "(" ++ first ++ " " ++ xs
    insert _ xs = oops $ "cons only works with quotes (pair not supported yet): " ++ show xs

evalBuildinCons xs = oops $ "cons wrong number of arguments: " ++ show xs


evalBuildinEq :: [SchemeValue] -> Eval SchemeValue
evalBuildinEq (x:y:[]) = do
  x' <- eval x
  y' <- eval y
  pure $ SchemeBool $ x' == y'
evalBuildinEq xs = oops $ "eq? wrong number of arguments: " ++ show xs

evalLambda :: SchemeValue -> Eval SchemeValue
evalLambda (SchemeLambda (Lambda ps ds b)) = do
  currDefs <- get
  pure $ SchemeLambda $ Lambda ps (ds++currDefs) b


eval :: SchemeValue -> Eval SchemeValue
eval v@(SchemeBool _) = pure v
eval v@(SchemeInteger _) = pure v
eval v@(SchemeDouble _) = pure v
eval v@(SchemeString _) = pure v
eval v@(SchemeSymbol _) = pure v
eval v@(SchemeQuote _) = pure v
eval v@(SchemeLambda _) = evalLambda v
eval v@(SchemeIf _ _ _) = evalIf v
eval v@(SchemeCond _) = evalCond v
eval v@(SchemeDefinition _) = evalDefinition v
eval v@(SchemeVariable _) = evalSynonym v
eval v@(SchemeFunctionCall _ _) = evalFunctionCall v
eval v@(SchemeLambdaCall _ _) = evalLambdaCall v

evalRec :: [SchemeValue] -> Eval [SchemeValue]
evalRec xs = sequenceA $ map eval xs


defaultDefs =
  [ Definition "else" $ Lambda [] [] (SchemeBool True)
  ]


main :: IO ()
main = do
  x <- readFile "test/example2.scm"
  putStrLn $
    case many schemeP `runParser` x of
      Just (s,_) ->
        case runEval (evalRec s) defaultDefs of
          Right (_, res) -> disp res
          Left err       -> err
      Nothing -> "didn't parse"
  return ()


disp :: [SchemeValue] -> String
disp (SchemeDefinition _:xs) = disp xs
disp (x:xs) = show x ++ "\n" ++ disp xs
disp [] = ""

