{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Alchemy where

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
combineElements i1 i2 g
  | i1 == i2 = error "Impossible to combine an element with itself"
  | otherwise =
  let all         = g ^. allElements
      desk        = g ^. deskElements
      oldHistory  = g ^. history

      e1          = desk !! i1
      e2          = desk !! i2
      newRoot     = (e1 ^. name, e2 ^. name)
      justNewRoot = Just newRoot

      matched     = filter (\e -> justNewRoot == (e ^. root)) all
      noMatches   = null matched

      newDesk     = if noMatches then desk
                    else filter (\e -> e `notElem` [e1, e2]) desk

      newHistory  = if noMatches then oldHistory
                    else oldHistory ++ [HistoryRecord newRoot (head matched)]

  in g & openedElements .~ (g ^. openedElements ++ matched)
       & deskElements   .~ newDesk ++ matched
       & history        .~ newHistory
