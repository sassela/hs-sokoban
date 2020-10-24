module Main where

import           CodeWorld

picture :: Picture
picture = maze

data Coordinates = Coords Integer Integer
data Direction = U | R | D | L
data Tile = Wall | Ground | Storage | Box | Blank

maze :: Picture
maze = pictures $ do
    x <- [-10..10]
    y <- [-10..10]
    pure $ placeTile x y
    where
        placeTile :: Integer -> Integer -> Picture
        placeTile x y =
            translated (fromIntegral x) (fromIntegral y) (drawTile (mazeTileAt x y))

mazeTileAt :: Integer -> Integer -> Tile
mazeTileAt x y
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
