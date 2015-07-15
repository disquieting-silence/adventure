module Item where

import Data.List
import Data.Char
import Rooms
import Data.Map
import Data.Maybe

data Item = ItemKey | ItemCrowbar deriving (Show, Eq, Ord)

newtype ItemName = ItemName String deriving (Show, Eq)
newtype ItemDesc = ItemDesc String deriving (Show, Eq)

type ItemCollection = Map Item ItemInfo

-- Still working on getting items to be a map ... but in the future, I'll try and hide the detail first
-- so that I can change its data structure representation more easily.

data ItemInfo = ItemInfo {
  getItem :: Item,
  getName :: ItemName,
  getRoom :: Maybe Room,
  getDesc :: ItemDesc
} deriving (Show, Eq)

findInfo :: ItemCollection -> Item -> Maybe ItemInfo
findInfo items item = Data.Map.lookup item items

showItem :: ItemInfo -> String
showItem (ItemInfo _ (ItemName name) _ _) = name


eqIgnoreCase :: String -> String -> Bool
eqIgnoreCase s1 s2 = (Data.List.map toUpper s1) == (Data.List.map toUpper s2)

--- Need to fix items.

getItemByName :: ItemCollection -> String -> Maybe ItemInfo
getItemByName items name = find (\(ItemInfo _ (ItemName n) _ _) -> eqIgnoreCase n name) (Data.Map.elems items)

toCollection :: [ItemInfo] -> ItemCollection
toCollection = Data.Map.fromList . (Data.List.map (\item@(ItemInfo k _ _ _ ) -> (k, item)))

hydrate :: [Item] -> ItemCollection -> ItemCollection
hydrate keys collection = Data.Map.fromList (Data.List.foldr (\a b -> maybe b (\v -> (a,v):b) (Item.findInfo collection a)) [] keys)

changeItem :: ItemCollection -> ItemInfo -> (ItemInfo -> ItemInfo) -> ItemCollection
changeItem items item f = Data.Map.map (\i -> if (item == i) then (f i) else i) items

pickupItemFromRoom :: ItemInfo -> ItemInfo
pickupItemFromRoom (ItemInfo k n _ d) = ItemInfo k n Nothing d

listItems :: ItemCollection -> [Item] -> String
listItems _ [] = "You have nothing in your inventory."
listItems infos items =
     let found = Data.Maybe.mapMaybe (Item.findInfo infos) items
         itemDescs = Data.List.map Item.showItem found
     in Data.List.intercalate "\n -- " $ [ "Inventory: \n" ] ++ itemDescs

dropItemInRoom :: Room -> ItemInfo -> ItemInfo
dropItemInRoom room (ItemInfo k n _ d) = ItemInfo k n (Just room) d

itemInRoom :: Room -> ItemInfo -> Bool
itemInRoom room item = maybe False (\r -> r == room) (Item.getRoom item)

