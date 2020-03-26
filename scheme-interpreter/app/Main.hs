module Main where

import System.Console.Haskeline
import System.Environment
import Interpreter (State, interpretFile, runExpression, defaultDefs)

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

main :: IO ()
main = do
    lib <- interpretFile "lib/prelude.pisp"
    case lib of
        Right (defs, _)-> do
            runInputT mySettings $ loop $ defs ++ defaultDefs
        Left err -> putStrLn $ "err: the Prelude couldn't load: " ++ err
    
    where
    loop :: State -> InputT IO ()
    loop defs = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just ":quit" -> return ()
            Just line -> do
                let (res, defs') = runExpression line defs
                outputStr res
                loop defs'

