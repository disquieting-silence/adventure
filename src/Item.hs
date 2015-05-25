module Item where

import Data.List
import Rooms

data Item = ItemKey | ItemCrowbar deriving (Show, Eq)

newtype ItemName = ItemName String deriving Show
newtype ItemDesc = ItemDesc String deriving Show

data ItemInfo = ItemInfo {
  getItem :: Item,
  getName :: ItemName,
  getRoom :: Maybe Room,
  getDesc :: ItemDesc
} deriving Show

findInfo :: [ItemInfo] -> Item -> Maybe ItemInfo
findInfo items item = find (\(ItemInfo i _ _ _) -> i == item) items

showItem :: ItemInfo -> String
showItem (ItemInfo _ (ItemName name) _ _) = name
