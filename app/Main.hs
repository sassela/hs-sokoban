module Main where

import           CodeWorld

picture :: Picture
picture = maze

data Coordinates = Coords Integer Integer
data Direction = U | R | D | L
data Tile = Wall | Ground | Storage | Box | Blank

initialCoords :: Coordinates
initialCoords = Coords 0 0

adjacentCoord :: Direction -> Coordinates -> Coordinates
adjacentCoord R (Coords x y) = Coords (x+1) y
-- example: handle U, L, D inputs
adjacentCoord U (Coords x y) = Coords  x   (y+1)
adjacentCoord L (Coords x y) = Coords (x-1) y
adjacentCoord D (Coords x y) = Coords  x   (y-1)

maze :: Picture
maze = pictures $ do
    x <- [-10..10]
    y <- [-10..10]
    pure $ placeTile (Coords x y)
    where
        placeTile :: Coordinates -> Picture
        placeTile coords =
            placeAt coords (drawTile (mazeTileAt coords))
        placeAt :: Coordinates -> Picture -> Picture
        placeAt (Coords x y) pic = translated (fromIntegral x) (fromIntegral y) pic

mazeTileAt :: Coordinates -> Tile
mazeTileAt (Coords x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

wall :: Picture
wall = coloured brown (solidRectangle 1 1)

ground :: Picture
ground = coloured green (solidRectangle 1 1)

storage :: Picture
storage = coloured white (thickCircle 0.1 0.2) & ground

box :: Picture
box = coloured yellow (solidRectangle 0.7 0.7) & ground

main :: IO ()
main = drawingOf picture
