-- Inspired in https://en.wikibooks.org/wiki/Haskell/Classes_and_types

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

sumList:: [Float] -> Float -> Int -> Float
sumList list num index = list!!index + num

getFromTuple :: (Float, Float, Float) -> Int -> Float
getFromTuple (a, _, _) 0 = a
getFromTuple (_, a, _) 1 = a
getFromTuple (_, _, a) 2 = a

class Located a where getLocation :: a -> [Float]
class (Located a) => Movable a where setLocation :: [Float] -> a -> a
move :: (Movable a) => [Float] -> a -> a
move [] p = p
move movements p = setLocation newPos p where newPos = mapInd (sumList movements) (getLocation p)

data Point2D = Point2D {pointX :: Float, pointY :: Float} deriving (Show)
instance Located Point2D where getLocation p = [pointX p, pointY p]
instance Movable Point2D where setLocation newPos p = p { pointX = newPos!!0, pointY = newPos!!1 }

data Point4D = Point4D {positions :: (Float, Float, Float), time :: Float} deriving (Show)
instance Located Point4D where getLocation p = [getFromTuple (positions p) 0, getFromTuple (positions p) 1, getFromTuple (positions p) 2, time p]
instance Movable Point4D where setLocation newPos p = p { positions = (newPos!!0, newPos!!1, newPos!!2), time = newPos!!3 }


p2d :: Point2D
p2d = Point2D 1 2

p4d :: Point4D
p4d = Point4D (0, 1, 2) 10

main :: IO()
main = do
  putStrLn $ "p2d: " ++ show p2d
  putStrLn $ "p2d moved [1.2, 2.0]: " ++ (show $ move [1.2, 2.0] p2d)
  putStrLn $ ""
  putStrLn $ "p4d: " ++ show p4d
  putStrLn $ "p4d moved [0, 0, 2, -5]: " ++ (show $ move [0, 0, 2, -5] p4d)
