import Control.Monad

main :: IO()
main = do
  whenFn
  sequenceFn
  mapFn
  colorsFn

whenFn :: IO()
whenFn = do
  putStrLn "If you write a space this will stop"
  c <- getChar
  when (c /= ' ') $ do
    whenFn

sequenceFn :: IO()
sequenceFn = do
  putStrLn "Enter three lines"
  rs <- sequence [getLine, getLine, getLine, getLine]
  print rs

mapFn :: IO()
mapFn = mapM_ print ["a","b","c"]

colorsFn :: IO()
colorsFn = do
  let range = ['a'..'d']
  colors <- forM range (\a -> do
    putStrLn $ "Which color do you associate with the letter " ++ show a ++ "?"
    color <- getLine
    return color)
  putStrLn $ "The colors that you associate with " ++ (show range) ++ " are: "
  putStrLn $ show colors
