{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Alchemy
  ( initGame
  , addElementToDesk
  , selectElement
  , combineElements
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust, isNothing)
import Data.Yaml
import Lens.Micro ((&), (.~), (^.), (?~))
import Lens.Micro.TH (makeLenses)

-- types
type Root = (String, String)

data Element = Element
  { _name :: String
  , _desc :: String
  , _root :: Maybe Root
  } deriving (Eq, Show)

data HistoryRecord = HistoryRecord
  { _from :: Root
  , _to   :: Element
  } deriving Show

data Game = Game
  { _allElements           :: [Element]
  , _openedElements        :: [Element]
  , _deskElements          :: [Element]
  , _selectedElement       :: Maybe Element
  , _history               :: [HistoryRecord]
  } deriving Show

-- lenses

makeLenses ''Element
makeLenses ''HistoryRecord
makeLenses ''Game

-- functions

-- parse yaml file for all available elements
instance FromJSON Element where
  parseJSON (Object v) = Element <$> v  .: "name"
                                 <*> v  .: "desc"
                                 <*> v .:? "root"
  parseJSON _ = error "Can't parse Elements from file"

getElementsFromFile :: IO [Element]
getElementsFromFile = do
  rawYaml <- BS.readFile "app/elements.yaml"
  let result = Data.Yaml.decodeThrow rawYaml :: Maybe [Element]
  return $ fromJust result

-- get init game state
initGame :: IO Game
initGame = do
  elements <- getElementsFromFile
  let baseElements = filter (\e -> isNothing $ e ^. root) elements
  return $ Game elements baseElements baseElements Nothing []

-- add to desk on col 1
addElementToDesk :: Int -> Game -> Game
addElementToDesk i g =
  let element = (g ^. allElements) !! i
  in g & deskElements .~ (g ^. deskElements ++ [element])

-- select first element to merge
selectElement :: Int -> Game -> Game
selectElement i g =
  let element = (g ^. deskElements) !! i
  in g & selectedElement ?~ element

-- try combine two elements on desk
combineElements :: Int -> Int -> Game -> Game
combineElements i1 i2 g =
  let all       = g ^. allElements
      desk      = g ^. deskElements
      e1        = desk !! i1
      e2        = desk !! i2
      newRoot   = Just (e1 ^. name, e2 ^. name)
      matched   = filter (\e -> newRoot == (e ^. root)) all
      newDesk   = if null matched
                  then desk
                  else filter (\e -> e `notElem` [e1, e2]) desk
  in g & openedElements .~ (g ^. openedElements ++ matched)
       & deskElements   .~ newDesk ++ matched
