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
  let g'     = addElementToDesk 0 g  -- test adding
      g''    = addElementToDesk 1 g
      g'''   = combineElements 0 1 g -- test success combine
      g''''  = combineElements 0 1 g -- test fail combine
      g''''' = selectElement 2 g'''' -- test selection
  pPrint g'''''
