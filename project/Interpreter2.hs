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



main :: IO ()
main = undefined
