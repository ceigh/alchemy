module Main where

-- import Brick
import Text.Pretty.Simple (pPrint)

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
  pPrint g''
