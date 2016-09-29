import Control.Applicative

maybeFooBar :: Maybe String
maybeFooBar = (++) <$> Just "foo" <*> Just "bar"

handleApplicatives f (a,b) = f <$> a  <*> b

printWithShow :: (Show a) => a -> IO()
printWithShow a = putStrLn $ (show a)

main :: IO()
main = do
  printWithShow $ [(*3),(+1),(^3)] <*> [1,2,3] -- (3 x 3) -> [3,6,7,2,3,4,1,8,27]
  printWithShow $ [(+),(*)] <*> [1,2] <*> [3,4] -- (2 x 2 x 2) -> [4,5,5,6,3,4,6,8]
  printWithShow maybeFooBar -- Just "foobar"
  printWithShow $ handleApplicatives (++) (Just "foo", Just "bar") -- Just "foobar"
  printWithShow $ handleApplicatives (++) (Just "foo", Nothing) -- Nothing
  printWithShow $ handleApplicatives (++) ([["b"]], [["a"]]) -- [["a", "b"]]