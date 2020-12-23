{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Alchemy
  ( initGame
  , addElementToFstCol
  , selectElement
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Yaml
import Lens.Micro ((&), (.~), (^.), (?~))
import Lens.Micro.TH (makeLenses)

-- types
type Root = (String, String)

data Element = Element
  { _name :: String
  , _desc :: String
  , _root :: Maybe Root
  } deriving Show

data HistoryRecord = HistoryRecord
  { _from :: Root
  , _to   :: Element
  } deriving Show

data Game = Game
  { _openedElements        :: [Element]
  , _allElements           :: [Element]
  , _fstColElements        :: [Element]
  , _sndColElements        :: [Element]
  , _fstColSelectedElement :: Maybe Element
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
  return $ Game [] elements [] [] Nothing []

-- add to desk on col 1
addElementToFstCol :: Int -> Game -> Game
addElementToFstCol i g =
  let element = (g ^. allElements) !! i
  in g & fstColElements .~ (g ^. fstColElements ++ [element])

-- select first element to merge
selectElement :: Int -> Game -> Game
selectElement i g =
  let element = (g ^. fstColElements) !! i
  in g & fstColSelectedElement ?~ element
