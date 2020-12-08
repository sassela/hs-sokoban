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

data SSState world
  = StartScreen
  | Running world

data Activity world =
  Activity world
           (Event -> world -> world)
           (world -> Picture)

startScreenActivityOf ::
     world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
startScreenActivityOf state0 handle draw = activityOf state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw) = Activity state0 handle' draw
  where
    handle' (KeyPress key) _
      | key == "Esc" = state0
    handle' e s = handle e s

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

gameWon :: [Coordinates] -> Bool
gameWon cs = all isOnStorage cs

isOnStorage :: Coordinates -> Bool
isOnStorage c =
  case (mazeTileAt c []) of
    Storage -> True
    _       -> False

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

sokoban :: Activity State
sokoban = Activity initialState updateState drawState

type Coordinates = (Double, Double)

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank

resetableActivityOf ::
     world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableActivityOf = undefined

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

initialState :: State
initialState = State initialCoords initialBoxes

showWin :: [Coordinates] -> Picture
showWin cs
  | gameWon cs = scaled 2 2 (lettering "You won!")
  | otherwise = blank

drawState :: State -> Picture
drawState (State playerPosition boxPositions) =
  pictures
    [showWin boxPositions, placeAt player playerPosition, maze boxPositions]

updateState :: Event -> State -> State
updateState _ (State c bx)
  | gameWon bx = (State c bx)
updateState (KeyPress "Up") state = movePlayer U state
updateState (KeyPress "Right") state = movePlayer R state
updateState (KeyPress "Down") state = movePlayer D state
updateState (KeyPress "Left") state = movePlayer L state
updateState _ state = state

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
main = runActivity (resetable (withStartScreen sokoban))
