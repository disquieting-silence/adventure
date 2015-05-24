module Movement where

import Direction
import Rooms
import Data.List

data Transition = Transition Room Direction Room deriving Show


matches :: Room -> Direction -> Transition -> Bool
matches room dir (Transition troom tdir _) = room == troom && dir == tdir 


move :: [Transition] -> Room -> Direction -> Maybe Room
move transitions current dir =
  let transition = find (matches current dir) transitions
  in fmap (\(Transition _ _ t) -> t) transition


doMove :: [Transition] -> Room -> Direction -> (String, [Transition], Room)
doMove transitions current dir =
  let dest = move transitions current dir
  in maybe 
    ("You cannot move " ++ (show dir) ++ ".", transitions, current)
    (\newroom -> ("Moving " ++ (show dir) ++ ".", transitions, newroom))
     dest

getExits :: [Transition] -> Room -> [Direction]
getExits transitions current = 
  let moves = filter (\(Transition s _ _) -> s == current) transitions
  in map (\(Transition _ dir _) -> dir) moves 
