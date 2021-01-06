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
  let g1 = addElementToDesk 0 g    -- test adding
      g2 = addElementToDesk 1 g1
      g3 = combineElements  0 1 g2 -- test success combine
      g4 = combineElements  0 1 g3 -- test fail combine
      g5 = selectElement    2 g4   -- test selection
      g6 = combineElements  0 1 g5 -- test fail combine
      g7 = combineElements  0 0 g6 -- test error combine
  pPrint g7
