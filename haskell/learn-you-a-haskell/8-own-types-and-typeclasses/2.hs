import Data.Typeable

-- Type synonyms
type Booleano = Bool

returnOppositeBooleano :: Booleano -> Booleano
returnOppositeBooleano val = not val

main = do
  let
    b2 = returnOppositeBooleano True
  print $ (typeOf b2) == (typeOf True)
