module Main where

import World
import Rooms
import Actions
import Direction
import Movement
import Player
import Control.Monad.Writer
import Control.Monad.State

gameWorld = World 
   [
     RoomInfo R1 (RoomName "Kitchen") (RoomDesc "It is a small room with white walls. There is the faint odour of something rotten."),
     RoomInfo R2 (RoomName "Dining Room") (RoomDesc "The table has been prepared for a banquet of some kind."),
     RoomInfo R3 (RoomName "Lounge") (RoomDesc "The couch is in the centre of the room."),
     RoomInfo R4 (RoomName "Ballroom") (RoomDesc "The room looks abandoned. There must not have been many balls for quite some time."),
     RoomInfo R5 (RoomName "Rumpus Room") (RoomDesc "No-one could have any fun here."),
     RoomInfo R6 (RoomName "Hall") (RoomDesc "There are portraits all over the walls."),
     RoomInfo R7 (RoomName "Conservatory") (RoomDesc "The plants seem to be dying."),
     RoomInfo R8 (RoomName "Library") (RoomDesc "It would take a lifetime to read all of these books."),
     RoomInfo R9 (RoomName "Study") (RoomDesc "The room is very quiet.") 
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
  ((result, log), (_, _)) <- (runStateT $ runWriterT (playGame turns)) (gameWorld, PlayerState R1 [])
  let actions = lines log
      counter = take (length actions) [1..]
      steps = zipWith (\n msg -> (show n) ++ ". " ++ msg) counter actions
  putStrLn $ "\nComplete Story: \n" ++ (unlines steps)
  return ()


main :: IO ()
main = runGame 2
