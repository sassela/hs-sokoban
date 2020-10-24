module Main where

import           CodeWorld

picture :: Picture
picture = drawTile Wall

data Tile = Wall | Ground | Storage | Box | Blank

-- Exercise
-- Create a function `drawTile :: Tile -> Picture` that returns a tile according to the following numbers:
-- 1. wall
-- 2. ground
-- 3. storage
-- 4. box
-- If the argument is not 1-4, `drawTile` should not draw anything, but should also not crash.
-- Tip: replace `picture = wall` with `picture = drawTile Wall`. The two statements should be equivalent
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
