module Rooms where

import Data.List

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq)

newtype RoomDesc = RoomDesc String deriving Show
newtype RoomName = RoomName String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getName :: RoomName,
  getDesc :: RoomDesc
} deriving Show


findInfo :: [RoomInfo] -> Room -> Maybe RoomInfo
findInfo rooms room = find (\info -> (getRoom info) == room) rooms

getDetailOf :: RoomInfo -> [String]
getDetailOf info =
  let (RoomName name) = getName info
      (RoomDesc desc) = getDesc info
  in ["You are in the " ++ name ++ ". " ++ desc]

getDetail :: [RoomInfo] -> Room -> [String]
getDetail rooms room =
  let info = findInfo rooms room
  in maybe ["I do not know where you are."] getDetailOf info
