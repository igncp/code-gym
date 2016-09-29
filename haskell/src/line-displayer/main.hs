import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import System.Process (callCommand)

import Options (Options, getOptions, file, shouldNotWait, delaySeconds)

printLineAndWait :: Options -> String -> IO ()
printLineAndWait opts line = do
  case line of
    "" -> return ()
    _ -> do 
      callCommand "clear"
      putStrLn ""
      putStrLn line
      putStrLn "\n\n"
      if (shouldNotWait opts) == False
        then threadDelay $ (delaySeconds opts) * 1000000
        else do
          _ <- getLine
          return ()

main :: IO ()
main = do
  args <- getArgs
  let opts = getOptions args
  fileContent <- readFile (file opts)
  let fileLines = lines fileContent
  mapM_ (printLineAndWait opts) fileLines
