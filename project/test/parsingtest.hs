import Parser (runParser,schemeP) 
import Control.Applicative (many)

main :: IO ()
main = do 
  x <- readFile "test/example.scm" 
  putStrLn $ 
    case many schemeP `runParser` x of
      Just s -> show s
      Nothing -> "kur"
  return ()



