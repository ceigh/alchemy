{-# LANGUAGE TemplateHaskell #-}
module Alchemy where

import Lens.Micro.TH (makeLenses)

-- types

data Element = Element
  { _name :: String
  , _desc :: String
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
  , _fstColSelectedElement :: Element
  , _history               :: [HistoryRecord]
  } deriving Show

makeLenses ''Element
makeLenses ''HistoryRecord
makeLenses ''Game
