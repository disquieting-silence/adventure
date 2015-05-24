module Rooms where

import Data.List

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq)

newtype RoomDesc = RoomDesc String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getDesc :: RoomDesc
} deriving Show


findInfo :: [RoomInfo] -> Room -> Maybe RoomInfo
findInfo rooms room = find (\(RoomInfo r d) -> r == room) rooms
