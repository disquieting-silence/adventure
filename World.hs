module World where

import Data.List
import Control.Monad.State
import Control.Monad.Writer

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq)

newtype RoomDesc = RoomDesc String deriving Show

data RoomInfo = RoomInfo Room RoomDesc deriving Show

data Direction = North | South | East | West deriving (Show, Eq)

data Transition = Transition Room Direction Room deriving Show

data PlayerState = PlayerState Room deriving Show

data GameOutcome = Win | Lose deriving Show

data World = World {
  getRooms :: [RoomInfo],
  getTransitions :: [Transition]
} deriving Show

matches :: Room -> Direction -> Transition -> Bool
matches room dir (Transition troom tdir _) = room == troom && dir == tdir 


move :: World -> Room -> Direction -> Maybe Room
move world current dir =
  let transitions = getTransitions world
      transition = find (matches current dir) transitions
  in fmap (\(Transition _ _ t) -> t) transition


doMove :: (World, PlayerState) -> Direction -> (String, World, PlayerState)
doMove (w, s@(PlayerState current)) dir =
  let dest = move w current dir
  in maybe 
    ("Cannot move there", w, s)
    (\newroom -> ("Moving ...", w, (PlayerState newroom)))
     dest


gameWorld = World 
   [
     RoomInfo R1 (RoomDesc "Kitchen") 
   ]
   [
     Transition R1 South R2
   ]

type App a = WriterT String (StateT (World, PlayerState) IO) a

playGame :: [Direction] -> State (World, PlayerState) GameOutcome
playGame [] = do
     (world, player) <- get
     return Lose
playGame (d:ds) = do
     state <- get
     let (message, world, ps) = doMove state d
     put (world, ps)
     playGame ds
-- |
-- Testing moving south from R1
-- >>> move gameWorld R1 South
-- Just R2
