{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Rooms where

import Data.List
import Data.Map(Map, lookup)
import GameCollection

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq, Ord)

data RoomCatalog = RoomCatalog [RoomInfo] deriving Show

createRooms :: [RoomInfo] -> RoomCatalog
createRooms rooms = RoomCatalog rooms

newtype RoomDesc = RoomDesc String deriving Show
newtype RoomName = RoomName String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getName :: RoomName,
  getDesc :: RoomDesc
} deriving Show

instance GameId Room where
  idToStrings room = [ "Key" ]

instance GameInfo RoomInfo where
  infoToStrings info =
    let (RoomName name) = getName info
        (RoomDesc desc) = getDesc info
    in ["You are in the " ++ name ++ ". " ++ desc]

instance GameGroup RoomInfo Room where
  toId = getRoom

instance GameCollection RoomCatalog RoomInfo Room where
  findFromId (RoomCatalog rooms) k = Data.List.find (\info -> (toId info) == k) rooms
