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

mySettings :: State -> Settings IO
mySettings defs = Settings 
    { historyFile = Just "myhist"
    , complete = completeWord Nothing " \t" $ \str -> do
        pure $ searchFunc defs str
    , autoAddHistory = True
    }


loop :: State -> IO ()
loop defs = do
    minput <- runInputT (mySettings defs) $ getInputLine "> "
    case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do
            let (res, defs') = runExpression line defs
            putStr res
            loop defs'

main :: IO ()
main = do
    lib <- interpretFile "lib/prelude.pisp"
    case lib of
        Right (defs, _)-> do
            loop $ defs ++ defaultDefs
        Left err -> putStrLn $ "err: the Prelude couldn't load: " ++ err



