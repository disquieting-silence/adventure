{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Uses where

import Data.Map
import Item
import World
import Player
import Direction
import Rooms
import GameCollection

-- the first driving use case of uses will be to use a 'key' which opens
-- up a new transition. Before using the key, the pathway will not be open.

type GameChanger = (World -> World, PlayerState -> PlayerState)
type UsageCollection = Map (Item, Room) GameChanger

openDoorFromR1ToR2 :: World -> World
openDoorFromR1ToR2 world =
  let transitions = World.getTransitions world
      newTransitions = maybe (Data.Map.insert (R1, South) R2 transitions) (const transitions) (Data.Map.lookup (R1, South) transitions)
     -- newTransitions = transitions
  in World (World.getRooms world) (World.getItems world) newTransitions


roomUses :: Map (Item, Room) (World -> World, PlayerState -> PlayerState)
roomUses = Data.Map.fromList
             [
               ((ItemKey, R1), (openDoorFromR1ToR2, id))
             ]


lookupUsage :: Item -> Room -> Maybe (World -> World, PlayerState -> PlayerState)
lookupUsage item room = Data.Map.lookup (item, room) roomUses
