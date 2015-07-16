module Movement where

import Direction
import Rooms
import Data.List
import Data.Map(Map,toList,lookup)
import Data.Maybe

type Transitions = Map (Room, Direction) Room


move :: Transitions -> Room -> Direction -> Maybe Room
move transitions current dir =
  Data.Map.lookup (current, dir) transitions

doMove :: Transitions -> Room -> Direction -> (String, Transitions, Room, Bool)
doMove transitions current dir =
  let dest = move transitions current dir
  in maybe 
    ("You cannot move " ++ (show dir) ++ ".", transitions, current, False)
    (\newroom -> ("Moving " ++ (show dir) ++ ".", transitions, newroom, True))
     dest

getExits :: Transitions -> Room -> [Direction]
getExits transitions current = foldr (\((r, d), _) b -> if r == current then d:b else b) [] (toList transitions) 
