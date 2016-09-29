import Control.Monad(unless)
import Data.Char

main :: IO ()
main = do
  let str = "Please, write some words and press enter"
  putStrLn str
  line <- getLine
  unless (null line) $ do
    putStrLn $ modifyLine line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

wrapper :: String
wrapper = "*--++"

addWrapper :: String -> String
addWrapper str = wrapper ++ str ++ reverse wrapper

modifyLine :: String -> String
modifyLine = addWrapper . reverseWords . map toUpper
