module Main where

import Game
import World
import Rooms
import Actions
import Direction
import Movement
import Player
import Item.Item
import Control.Monad.Writer
import Control.Monad.State
import Data.Map

gameWorld = World
   (createRooms [
     RoomInfo R1 (RoomName "Kitchen") (RoomDesc "It is a small room with white walls. There is the faint odour of something rotten."),
     RoomInfo R2 (RoomName "Dining Room") (RoomDesc "The table has been prepared for a banquet of some kind."),
     RoomInfo R3 (RoomName "Living Room") (RoomDesc "The couch is in the centre of the room."),
     RoomInfo R4 (RoomName "Ballroom") (RoomDesc "The room looks abandoned. There must not have been many balls for quite some time."),
     RoomInfo R5 (RoomName "Rumpus Room") (RoomDesc "No-one could have any fun here."),
     RoomInfo R6 (RoomName "Foyer") (RoomDesc "There are portraits all over the walls."),
     RoomInfo R7 (RoomName "Greenhouse") (RoomDesc "The plants seem to be dying."),
     RoomInfo R8 (RoomName "Library") (RoomDesc "It would take a lifetime to read all of these books."),
     RoomInfo R9 (RoomName "Study") (RoomDesc "The room is very quiet.")
   ])

   (createItems
     [
       ItemInfo ItemKey (ItemName "Key") (Just R1) (ItemDesc "The key is oddly-shaped and blue."),
       ItemInfo ItemCrowbar (ItemName "Crowbar") Nothing (ItemDesc "The crowbar is lean and silver.")
     ]
   )

   (Data.Map.fromList
     [
--	((R1, South), R2),
        ((R2, North), R1),
        ((R2, East), R5),
	((R2, South), R3),
	((R3, East), R6),
	((R3, North), R2),
	((R4, South), R5),
	((R5, North), R4),
	((R5, West), R2),
	((R6, West), R3),
	((R6, East), R9),
	((R7, South), R8),
	((R8, North), R7),
	((R8, South), R9),
	((R9, North), R8),
	((R9, West), R6)
     ])


runGame :: TurnsLeft -> IO ()
runGame turns = do
  ((result, log), (_, _)) <- (runStateT $ runWriterT (playGame turns)) (gameWorld, PlayerState R1 [])
  let actions = lines log
      counter = take (length actions) [1..]
      steps = zipWith (\n msg -> (show n) ++ ". " ++ msg) counter actions
  putStrLn $ "\nComplete Story: \n" ++ (unlines steps)
  return ()


main :: IO ()
main = runGame 50
