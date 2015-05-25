module Item where

import Rooms

data Item = ItemKey | ItemCrowbar deriving Show

newtype ItemName = ItemName String deriving Show
newtype ItemDesc = ItemDesc String deriving Show

data ItemInfo = ItemInfo {
  getItem :: Item,
  getName :: ItemName,
  getRoom :: Room,
  getDesc :: ItemDesc
} deriving Show

