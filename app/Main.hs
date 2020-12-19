module Main where

-- import Brick

import Alchemy (getElementsFromFile)

{-
ui :: Widget ()
ui = str getElementsFromFile
-}

main :: IO ()
-- main = simpleMain ui
main = do
  elements <- getElementsFromFile
  print elements
