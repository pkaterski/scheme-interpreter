{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE LambdaCase #-}

import Parser 

type State = [SchemeValue] -- the definitions

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
oops err = Eval \_ -> Left err

searchDefinition :: String -> [SchemeValue] -> Maybe SchemeValue
searchDefinition _ [] = Nothing
searchDefinition s (d@(SchemeDefinition s' _ _):ds) = 
  if s == s'
  then Just d
  else searchDefinition s ds
searchDefinition _ _ = error "impossible, state contains a non-definition"

isDefined :: String -> [SchemeValue] -> Bool
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
     

eval :: SchemeValue -> Eval SchemeValue
eval v@(SchemeBool _) = pure v
eval v@(SchemeInteger _) = pure v
eval v@(SchemeDouble _) = pure v
eval v@(SchemeString _) = pure v
eval v@(SchemeIf _ _ _) = evalIf v
eval v@(SchemeCond _) = evalCond v


main :: IO ()
main = undefined
