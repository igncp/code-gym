module Shapes
(
  Shape(..)
, surface
) where

-- Shape is a type
-- Circle and Rectangle are value constructors. The rest are fields
-- Show is a typeclass
data Shape = Circle {
  centre :: Point,
  radius :: Float
} | Rectangle Point Point deriving (Show)
data Point = Point {
  x :: Float,
  y :: Float
} deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

getCentre :: Shape -> Point
getCentre (Circle c r) = c
getCentre (Rectangle (Point x1 y1) (Point x2 y2)) = Point ((x1+x2)/2) ((y1+y2)/2)


main = do
  putStrLn "Enter the height of the rectangle: "
  heightS <- getLine
  putStrLn "Enter the width of the rectangle: "
  widthS <- getLine
  let
    width = (read widthS :: Float)
    height = (read heightS :: Float)
    rect = baseRect width height
  putStrLn $ "The rectangle: " ++ show rect
  putStrLn $ "Has centre is in: " ++ show (getCentre rect)

