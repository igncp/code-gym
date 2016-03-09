import Data.Char

main :: IO()
main = do
  str <- return "Please, write some words and press enter"
  putStrLn str
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ modifyLine line
      main

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words

wrapper :: String
wrapper = "*--++"

addWrapper :: String -> String
addWrapper str = wrapper ++ str ++ (reverse wrapper)

modifyLine :: String -> String
modifyLine = addWrapper . reverseWords . (map toUpper)
