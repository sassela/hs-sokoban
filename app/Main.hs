module Main where

import           CodeWorld

picture :: Picture
picture = storage

wall :: Picture
wall = coloured brown (solidRectangle 1 1)

-- Exercise:
-- Using `wall` as an example, create 3 more variables to represent tiles on a map: ground, storage and box.
-- Each variable should be of type Picture and have a width and height of 1.
-- Tip: replace `picture = wall` with `picture = <variable>` to preview each tile.
ground :: Picture
ground = coloured green (solidRectangle 1 1)

storage :: Picture
storage = coloured white (thickCircle 0.1 0.2) & ground

box :: Picture
box = coloured yellow (solidRectangle 0.7 0.7) & ground

main :: IO ()
main = drawingOf picture
