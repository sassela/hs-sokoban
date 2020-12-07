{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CodeWorld

data Direction
  = U
  | R
  | D
  | L

data State =
  State Coordinates
        [Coordinates]

type Coordinates = (Double, Double)

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank

initialState :: State
initialState = State initialCoords initialBoxes

drawState :: State -> Picture
drawState (State playerPosition boxPositions) =
  pictures [placeAt player playerPosition, maze boxPositions]

updateState :: Event -> State -> State
updateState (KeyPress "Up") state    = movePlayer U state
updateState (KeyPress "Right") state = movePlayer R state
updateState (KeyPress "Down") state  = movePlayer D state
updateState (KeyPress "Left") state  = movePlayer L state
updateState _ state                  = state

movePlayer :: Direction -> State -> State
movePlayer direction (State playerPosition boxPositions) =
  case mazeTileAt newPlayerPosition boxPositions of
    Box ->
      case mazeTileAt newBoxPosition boxPositions of
        Ground  -> move
        Storage -> move
        _       -> stay
    Ground -> move
    Storage -> move
    _ -> stay
  where
    newPlayerPosition = adjacentCoord direction playerPosition
    newBoxPosition = adjacentCoord direction newPlayerPosition
    move = State newPlayerPosition newBoxPositions
    newBoxPositions = map maybeMoveBox boxPositions
    stay = State playerPosition boxPositions
    maybeMoveBox currentBoxPosition
      | newPlayerPosition == currentBoxPosition = newBoxPosition
      | otherwise = currentBoxPosition

player :: Picture
player = solidCircle 0.3

initialCoords :: Coordinates
initialCoords = (0, -1)

initialBoxes :: [Coordinates]
initialBoxes = [(-2, 0), (-1, 0), (0, 0), (1, 0)]

placeAt :: Picture -> Coordinates -> Picture
placeAt picture (x, y) = translated x y picture

adjacentCoord :: Direction -> Coordinates -> Coordinates
adjacentCoord R (x, y) = (x + 1, y)
adjacentCoord U (x, y) = (x, y + 1)
adjacentCoord L (x, y) = (x - 1, y)
adjacentCoord D (x, y) = (x, y - 1)

mazeLimit :: Double
mazeLimit = 4

maze :: [Coordinates] -> Picture
maze boxCoords =
  pictures $ do
    x <- [-mazeLimit .. mazeLimit]
    y <- [-mazeLimit .. mazeLimit]
    pure $ placeAt (drawTile (mazeTileAt (x, y) boxCoords)) (x, y)

mazeTileAt :: Coordinates -> [Coordinates] -> Tile
mazeTileAt (x, y) boxCoords
  | elem (x, y) boxCoords = Box
  | mazeBorder = Wall
  | innerWall = Wall
  | storageUnits = Storage
  | otherwise = Ground
  where
    mazeBorder = abs x == mazeLimit || abs y == mazeLimit
    innerWall = elem (x, y) [(2, 0), (2, -1), (2, -2), (2, -3)]
    storageUnits = elem (x, y) [(3, 0), (3, -1), (3, -2), (3, -3)]

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
main = activityOf initialState updateState drawState
