module Main where

import Brick
import Alchemy

ui :: String -> Widget ()
ui s = str s

main :: IO ()
main = do
  g <- initGame
  simpleMain $ ui $ show g
