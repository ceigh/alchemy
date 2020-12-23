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
  print g
  -- print $ addElementToFstCol (Element "a" "b" (Just ("a", "b"))) g
  -- print $ selectElement (Element "a" "b" (Just ("a", "b"))) g
