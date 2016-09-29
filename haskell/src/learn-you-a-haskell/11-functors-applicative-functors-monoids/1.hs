import System.Environment
import Data.List
import Data.Char

-- IO is an instance of the Functor typeclass

main :: IO()
main = do
  args <- getArgs
  if not (null args) then do
    line <- fmap (modifyStr . head) getArgs
    printMessage line
  else putStrLn "You must provide an argument"

modifyStr :: String -> String
modifyStr = intersperse '-' . sort . nub . map toLower

printMessage :: String -> IO()
printMessage line = putStrLn $ "Your text after the transformation is: " ++ line
