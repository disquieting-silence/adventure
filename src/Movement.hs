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
    ("Cannot move there", transitions, current)
    (\newroom -> ("Moving ...", transitions, newroom))
     dest

