module Main where

-- import Brick

import Alchemy (initGame)

{-
ui :: Widget ()
ui = str getElementsFromFile
-}

main :: IO ()
-- main = simpleMain ui
main = do
  game <- initGame
  print game
