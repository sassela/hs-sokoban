module Main where 
    
import CodeWorld

picture :: Picture
picture = wall

wall :: Picture
wall = coloured brown (solidRectangle 1 1)

main :: IO ()
main = drawingOf picture
