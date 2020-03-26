{-# OPTIONS_GHC -W #-}

module Main where

import Interpreter (interpretFile, runREPL, defaultDefs)
import System.IO

main :: IO ()
main = do
  lib <- interpretFile "lib/prelude.pisp"
  case lib of
    Right (defs, _)-> do
      hSetBuffering stdout NoBuffering
      runREPL $ defs ++ defaultDefs
    Left err -> putStrLn $ "err: the Prelude couldn't load: " ++ err
