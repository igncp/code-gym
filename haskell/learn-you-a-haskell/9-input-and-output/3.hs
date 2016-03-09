import System.Environment

main :: IO()
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn $ "The arguments are: " ++ (show args)
   putStrLn $ "The program name is: " ++ progName
   handleArgs args

handleArgs :: [String] -> IO()
handleArgs args
  | hasFoo && hasBar = putStrLn "FOOBAR"
  | hasFoo = putStrLn "FOO"
  | hasBar = putStrLn "BAR"
  | otherwise = putStrLn "Not a valid flag provided. Valid flags are `--foo` and `--bar`"
  where
    hasFoo = elem "--foo" args
    hasBar = elem "--bar" args