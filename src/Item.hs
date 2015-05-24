module Item where

import Rooms

data Item = ItemKey | ItemCrowbar deriving Show

data ItemInfo = ItemInfo {
  getName :: String,
  getRoom :: Room,
  getDesc :: String
}

