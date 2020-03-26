module Main where

import System.Console.Haskeline
import System.Environment
import Interpreter (State, Eval, interpretFile, get, runExpression, defaultDefs)
import Parser (Definition(..))
import Data.List

searchFunc :: State -> String -> [Completion]
searchFunc defs str =
    map simpleCompletion $ filter (str `isPrefixOf`) $ defsToNames defs

defsToNames :: State -> [String]
defsToNames = map $ \(Definition name _) -> name

mySettings :: Settings IO
mySettings = Settings 
    { historyFile = Just "myhist"
    , complete = completeWord Nothing " \t" $ \str -> do
        -- defs <- get
        pure $ searchFunc defaultDefs str
    , autoAddHistory = True
    }

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
        -- defs <- get
        case minput of
            Nothing -> return ()
            Just ":quit" -> return ()
            Just line -> do
                let (res, defs') = runExpression line defs
                outputStr res
                loop defs'

