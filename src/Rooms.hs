{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}


module Rooms where

import Data.List

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq, Ord)

newtype RoomDesc = RoomDesc String deriving Show
newtype RoomName = RoomName String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getName :: RoomName,
  getDesc :: RoomDesc
} deriving Show

class Eq raw => GameId raw where
  gDeref :: raw

class GameInfo i where
  gGetKey :: GameId k => i -> k
  gLog :: i -> [String]


newtype GameError = GameError String

showError :: GameError -> [String]
showError _ = [ "dog" ]

gGetDetail :: (GameInfo i, GameId k) => [i] -> k -> GameError -> [String]
gGetDetail infos key err =
  let mInfo = find (\i -> (gGetKey i) == key) infos
  in maybe (showError err) gLog mInfo
-- class Eq k => DetailedInfo i k e where
--   sfindInfo :: [i] -> k -> Maybe i
--   sfindInfo infos key = find (\info -> (sgetKey info) == key) infos
--   sgetKey :: i -> k
--   sgetDetailOf :: i -> [String]
--   sgetDetail :: [i] -> k -> e -> [String]
--   sgetDetail infos key err =
--     let info = sfindInfo infos key
--     in maybe (errMessage err) (\_ -> ["Cat"]) info
--   -- sgetDetail infos key =
--   --   let info = sfindInfo infos key
--   --   in maybe (errMessage key) sgetDetailOf info
--   stoString :: i -> [String]
--   errMessage :: e -> [String]

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
