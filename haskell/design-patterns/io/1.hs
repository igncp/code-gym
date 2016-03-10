import Data.List

handleIOs :: [IO()] -> IO()
handleIOs = foldr (>>) (return ())

ios :: [IO()]
ios = map putStr $ intersperse " " ["this", "won't", "run"]

main :: IO()
main = do
  putStrLn "A laziness behaviour:"
  putStrLn "until ios variable is 'handled'..."
  handleIOs ios
  putStrLn ""
