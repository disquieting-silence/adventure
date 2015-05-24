module Main where

import World
import Rooms
import Actions
import Direction
import Movement
import Control.Monad.Writer
import Control.Monad.State

gameWorld = World 
   [
     RoomInfo R1 (RoomDesc "Kitchen"),
     RoomInfo R2 (RoomDesc "Dining Room"),
     RoomInfo R3 (RoomDesc "Lounge"),
     RoomInfo R4 (RoomDesc "Ballroom"),
     RoomInfo R5 (RoomDesc "Rumpus Room"),
     RoomInfo R6 (RoomDesc "Hall"),
     RoomInfo R7 (RoomDesc "Conservatory"),
     RoomInfo R8 (RoomDesc "Library"),
     RoomInfo R9 (RoomDesc "Study") 
   ]
   [
     Transition R1 South R2,
     Transition R2 North R1,
     Transition R2 East R5,
     Transition R2 South R3,
     Transition R3 East R6,
     Transition R3 North R2,
     Transition R4 South R5,
     Transition R5 North R4,
     Transition R5 West R2,
     Transition R6 West R3,
     Transition R6 East R9,
     Transition R7 South R8,
     Transition R8 North R7,
     Transition R8 South R9,
     Transition R9 North R8,
     Transition R9 West R6
   ]


runGame :: TurnsLeft -> IO ()
runGame turns = do 
  (runStateT $ runWriterT (playGame turns)) (gameWorld, PlayerState R1)
  return ()


main :: IO ()
main = runGame 2
