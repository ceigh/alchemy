{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Alchemy
  ( initGame ) where

import Control.Applicative
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import Lens.Micro.TH (makeLenses)

-- types

data Element = Element
  { _name :: String
  , _desc :: String
  , _root :: Maybe (String, String)
  } deriving Show

data HistoryRecord = HistoryRecord
  { _from :: Element
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
