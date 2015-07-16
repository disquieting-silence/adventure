module Player where

import Item
import Rooms

import Data.Map(fromList)

data PlayerState = PlayerState {
  getRoom :: Room,
  getItems :: [Item] 
} deriving Show

updateRoom :: PlayerState -> Room -> PlayerState
updateRoom (PlayerState room items) newRoom = PlayerState newRoom items

addItemToPlayer :: Item -> PlayerState -> PlayerState
addItemToPlayer itemId player =
  let newPlayerItems = itemId : (Player.getItems player)
  in PlayerState (Player.getRoom player) newPlayerItems

dropItemFromPlayer :: Item -> PlayerState -> PlayerState
dropItemFromPlayer itemId player =
  let (_, newItems) = foldr (cacheAndFind (== itemId)) (Nothing, []) (Player.getItems player)
  in PlayerState (Player.getRoom player) newItems

cacheAndFind :: (a -> Bool) -> a -> (Maybe a, [a]) -> (Maybe a, [a])
cacheAndFind pred x (ox, xs) =
  -- the goal here is to find something in the list and store it, while filtering it out
  -- this is going to be the fold function's operator
  let include = pred x
      list = if include then xs else x:xs
      cached = maybe (if include then (Just x) else Nothing) (const ox) ox
  in (cached, list)


-- |
-- Testing add an item to a player
-- >>> addItemToPlayer ItemKey (PlayerState {Player.getRoom = R1, getItems = []})
-- PlayerState {getRoom = R1, getItems = [ItemKey]}

