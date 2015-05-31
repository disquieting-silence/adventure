module Item where

import Data.List
import Data.Char
import Rooms

data Item = ItemKey | ItemCrowbar deriving (Show, Eq)

newtype ItemName = ItemName String deriving (Show, Eq)
newtype ItemDesc = ItemDesc String deriving (Show, Eq)

data ItemInfo = ItemInfo {
  getItem :: Item,
  getName :: ItemName,
  getRoom :: Maybe Room,
  getDesc :: ItemDesc
} deriving (Show, Eq)

findInfo :: [ItemInfo] -> Item -> Maybe ItemInfo
findInfo items item = find (\(ItemInfo i _ _ _) -> i == item) items

showItem :: ItemInfo -> String
showItem (ItemInfo _ (ItemName name) _ _) = name


eqIgnoreCase :: String -> String -> Bool
eqIgnoreCase s1 s2 = (map toUpper s1) == (map toUpper s2)

getItemByName :: [ItemInfo] -> String -> Maybe ItemInfo
getItemByName items name = find (\(ItemInfo _ (ItemName n) _ _) -> eqIgnoreCase n name) items
