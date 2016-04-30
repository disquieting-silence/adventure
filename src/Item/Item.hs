{-# LANGUAGE MultiParamTypeClasses #-}

module Item.Item where

import Data.List
import Data.Char
import Rooms
import Data.Map
import Data.Maybe
import GameCollection

data Item = ItemKey | ItemCrowbar deriving (Show, Eq, Ord)

newtype ItemName = ItemName String deriving (Show, Eq)
newtype ItemDesc = ItemDesc String deriving (Show, Eq)

newtype ItemCatalog = ItemCatalog (Map Item ItemInfo) deriving Show

createItems :: [ItemInfo] -> ItemCatalog
createItems items =
  let paired = Data.List.map getGroup items
      map = Data.Map.fromList paired
  in ItemCatalog map

instance GameId Item where
  idToStrings i = [ show i ]

instance GameInfo ItemInfo where
  infoToStrings (ItemInfo _ (ItemName name) _ _) = [ name ]

instance GameCollection ItemCatalog ItemInfo Item where
  findFromId (ItemCatalog map) k = Data.Map.lookup k map

-- Still working on getting items to be a map ... but in the future, I'll try and hide the detail first
-- so that I can change its data structure representation more easily.

data ItemInfo = ItemInfo {
  getItem :: Item,
  getName :: ItemName,
  getRoom :: Maybe Room,
  getDesc :: ItemDesc
} deriving (Show, Eq)

eqIgnoreCase :: String -> String -> Bool
eqIgnoreCase s1 s2 = (Data.List.map toUpper s1) == (Data.List.map toUpper s2)


getGroup :: ItemInfo -> (Item, ItemInfo)
getGroup info = (getItem info, info)

--- Need to fix items.

getItemByName :: ItemCatalog -> String -> Maybe ItemInfo
getItemByName (ItemCatalog items) name = find (\(ItemInfo _ (ItemName n) _ _) -> eqIgnoreCase n name) (Data.Map.elems items)

hydrate :: [Item] -> ItemCatalog -> ItemCatalog
hydrate keys collection =
  let list = Data.List.foldr (\a b -> maybe b (\v -> (a,v):b) (findFromId collection a)) [] keys
  in ItemCatalog $ Data.Map.fromList list

changeItem :: ItemCatalog -> ItemInfo -> (ItemInfo -> ItemInfo) -> ItemCatalog
changeItem (ItemCatalog items) item f =
  let raw = Data.Map.map (\i -> if (item == i) then (f i) else i) items
  in ItemCatalog raw

pickupItemFromRoom :: ItemInfo -> ItemInfo
pickupItemFromRoom (ItemInfo k n _ d) = ItemInfo k n Nothing d

listItems :: ItemCatalog -> [Item] -> String
listItems _ [] = "You have nothing in your inventory."
listItems infos items =
     let found = Data.Maybe.mapMaybe (findFromId infos) items
         itemDescs = Data.List.concatMap infoToStrings found
     in Data.List.intercalate "\n -- " $ [ "Inventory: \n" ] ++ itemDescs

dropItemInRoom :: Room -> ItemInfo -> ItemInfo
dropItemInRoom room (ItemInfo k n _ d) = ItemInfo k n (Just room) d

itemInRoom :: Room -> ItemInfo -> Bool
itemInRoom room info = maybe False (\r -> r == room) (Item.Item.getRoom info)

 -- Data.Maybe.mapMaybe (Item.findInfo allItems) current
filterItemsById :: ItemCatalog -> [Item] -> ItemCatalog
filterItemsById catalog keepers = createItems $ Data.Maybe.mapMaybe (findFromId catalog) keepers

filterItemsByInfoPred :: ItemCatalog -> (ItemInfo -> Bool) -> ItemCatalog
filterItemsByInfoPred (ItemCatalog items) f = ItemCatalog $ Data.Map.filter f items

mapItemInfos :: ItemCatalog -> (ItemInfo -> a) -> [a]
mapItemInfos (ItemCatalog items) f = Data.Map.elems $ Data.Map.map f items
