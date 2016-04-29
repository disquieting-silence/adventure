{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Rooms where

import Data.List
import Data.Map(Map, lookup)

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq, Ord)

newtype RoomDesc = RoomDesc String deriving Show
newtype RoomName = RoomName String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getName :: RoomName,
  getDesc :: RoomDesc
} deriving Show

{-
Start small ... I want to create something which allows me to
find something in a list of things where that thing has a mapping
between one thing and another. I honestly think the only way I'll be
able to do it is if I use a MultiParamTypeClasses extension

Let's try that again. This time, keep the signatures to only the things
that require both types ... use constraints for the types and put the signatures
in those constraining types



-}

class Eq k => SuccId k where
  klog :: k -> [ String ]

class SuccInfo i where
  ilog :: i -> [ String ]

class (SuccId k, SuccInfo i) => SuccGroup i k where
  succKey :: i -> k

class (SuccId k, SuccInfo i, SuccGroup i k) => SuccCollection c i k where
  succLookup :: c -> k -> Maybe i

instance (Ord k, SuccId k, SuccInfo i, SuccGroup i k) => SuccCollection (Map k i) i k where
  succLookup c k = Data.Map.lookup k c

instance (SuccId k, SuccInfo i, SuccGroup i k) => SuccCollection [i] i k where
  succLookup c k = find (\i -> (succKey i) == k) c


instance SuccId Room where
  klog room = [ "Key" ]

instance SuccInfo RoomInfo where
  ilog info = [ "Info" ]

instance SuccGroup RoomInfo Room where
  succKey = getRoom

-- class GameId k => InfoOf k where
--   xGetKey ::

newtype GameError = GameError String

showError :: GameError -> [String]
showError _ = [ "dog" ]

-- gGetDetail :: (GameInfo i, GameId k) => [i] -> k -> GameError -> [String]
-- gGetDetail infos key err =
--   let mInfo = find (\i -> (gGetKey i) == key) infos
--   in maybe (showError err) gLog mInfo
--
-- instance GameId Room


-- instance GameInfo RoomInfo where
--   gGetKey :: GameId k => RoomInfo -> k
--   gGetKey info = getRoom info

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
