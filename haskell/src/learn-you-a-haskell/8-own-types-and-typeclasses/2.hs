import Data.Typeable

-- Type synonyms
type Booleano = Bool

returnOppositeBooleano :: Booleano -> Booleano
returnOppositeBooleano = not

main :: IO ()
main = do
  let
    b2 = returnOppositeBooleano True
  print $ typeOf b2 == typeOf True
