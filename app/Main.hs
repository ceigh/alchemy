module Main where

-- import Brick

import Alchemy

{-
ui :: Widget ()
ui = str getElementsFromFile
-}

main :: IO ()
-- main = simpleMain ui
main = do
  g <- initGame
  let g'  = addElementToDesk 0 g
      g'' = selectElement    0 g'
  print g''
