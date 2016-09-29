import qualified Data.Map as Map

data CarColor = Red | Blue | Green deriving (Show, Eq, Ord)
data CarBrand = Honda | Fiat | Ford deriving (Show, Eq, Ord)

type PlateNumber = String
type CarSpec = (CarColor, PlateNumber)
type CarsMap = Map.Map CarBrand CarSpec

carLookup :: CarBrand -> CarsMap -> Either String CarSpec
carLookup carBrand m =
  case Map.lookup carBrand m of
    Nothing -> Left $ "Car of brand " ++ show carBrand ++ " is not present"
    Just (color, plate) -> Right (color, plate)

currentCars :: CarsMap
currentCars = Map.fromList [
  (Honda, (Red,"AAA000000"))
  , (Fiat, (Red,"BBB000000"))
  ]

getResultStr :: CarBrand -> String
getResultStr b = (show b) ++ ": " ++ (show $ carLookup b currentCars)

main :: IO()
main = do
  putStrLn $ "----"
  putStrLn $ getResultStr Honda
  putStrLn $ getResultStr Fiat
  putStrLn $ getResultStr Ford
  putStrLn $ "----"
