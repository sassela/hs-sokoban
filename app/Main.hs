module Main where 
    
import CodeWorld

picture :: Picture
picture = solidRectangle 1 1

main :: IO ()
main = drawingOf picture
