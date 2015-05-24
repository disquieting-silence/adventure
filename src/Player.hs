module Player where

import Item
import Rooms

data PlayerState = PlayerState {
  getRoom :: Room,
  getItems :: [Item] 
} deriving Show

updateRoom :: PlayerState -> Room -> PlayerState
updateRoom (PlayerState room items) newRoom = PlayerState newRoom items
