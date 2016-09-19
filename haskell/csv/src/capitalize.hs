-- This script accepts a CSV file with albums (see sample)
-- and it will capitalize all words in the authors and the
-- first word of the title (with some extra cleverness)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args
  putStrLn content
