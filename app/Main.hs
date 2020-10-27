{-# LANGUAGE OverloadedStrings #-}
module Main where

import           CodeWorld
import           CodeWorld.Sketches (sketchedRobot)

picture :: Picture
picture = maze

data Coordinates = Coords Integer Integer
data Direction = U | R | D | L
data State = State Coordinates [Coordinates]
data Tile = Wall | Ground | Storage | Box | Blank

initialState :: State
initialState = State initialCoords initialBoxes

drawState :: State -> Picture
drawState (State playerPosition boxPositions) =
    placeAt playerPosition player &
    drawBoxes boxPositions &
    maze

player :: Picture
player = scaled 0.2 0.2 sketchedRobot

initialCoords :: Coordinates
initialCoords = Coords 0 (-1)

initialBoxes :: [Coordinates]
initialBoxes = [Coords (-2) 0, Coords (-1) 0, Coords 0 0, Coords 1 0]

drawBoxes :: [Coordinates] -> Picture
drawBoxes coords = pictures (map (\c -> placeAt c (drawTile Box)) coords)

updateCoords :: Event -> Coordinates -> Coordinates
updateCoords (KeyPress key) coords
    | key == "Up"    = adjacentCoord U coords
    -- exercise: handle R, D, L keypress inputs
    | key == "Right" = adjacentCoord R coords
    | key == "Down"  = adjacentCoord D coords
    | key == "Left"  = adjacentCoord L coords
updateCoords _ coords      = coords

drawCoords :: Coordinates -> Picture
drawCoords coords = placeAt coords maze

placeAt :: Coordinates -> Picture -> Picture
placeAt (Coords x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coordinates -> Coordinates
adjacentCoord R (Coords x y) = Coords (x+1) y
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
main = drawingOf (drawState initialState)
