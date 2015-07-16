module Game where

import Data.List
import Data.Maybe
import Data.Map(fromList, elems, filter, map, mapMaybe)
import Control.Monad.State
import Control.Monad.Writer
import Rooms
import Actions
import Direction
import Movement
import Item
import Player
import Uses
import World

type App a = WriterT String (StateT (World, PlayerState) IO) a
data GameOutcome = Win | Lose deriving Show
 
type TurnsLeft = Int
 
 
type StateChangers = (World -> World, PlayerState -> PlayerState)

doAction :: TurnsLeft -> Action -> App GameOutcome
-- Handle movement
doAction turns (Move dir) = do
     (world, player) <- get
     let current = Player.getRoom player
         (message, newTransitions, ps, usedTurn) = doMove (getTransitions world) current dir
     tell $ message ++ "\n"
     liftIO $ putStrLn $ "\n" ++ message
     put (World (getRooms world) (World.getItems world) newTransitions, updateRoom player ps)
     playGame (if usedTurn then turns - 1 else turns)
-- Handle listing inventory
doAction turns Inventory = do
     (world, player) <- get
     let items = Player.getItems player
     liftIO $ putStrLn $ "\n" ++ (listItems (World.getItems world) items)
     playGame (turns - 1)
-- Handle quitting game
doAction turns Quit = playGame 0
-- Handle picking up objects
doAction turns (Pickup obj) = do
     (world, player) <- get
     let currentItems = itemsInRoom world (Player.getRoom player)
         specItem = getItemByName currentItems obj
     _ <- maybe (itemNotThere obj) (\i -> pickupItem i) specItem
     playGame (turns - 1)
-- Handle dropping objects
doAction turns (Drop obj) = do
     (world, player) <- get
     let specItem = getItemInInventory world player obj
     _ <- maybe (itemNotInInventory obj) dropItem specItem
     playGame (turns - 1)
doAction turns (Use obj) = do
     (world, player) <- get
     let specItem = getItemInInventory world player obj
     _ <- maybe (itemNotInInventory obj) useItem specItem
     playGame (turns - 1)


endTurn :: TurnsLeft -> String -> App GameOutcome
endTurn turns msg = do
     liftIO $ putStrLn ("I did not understand your command: \"" ++ msg ++ "\"")
     playGame turns

playGame :: TurnsLeft -> App GameOutcome
playGame 0 = return Lose
playGame turns = do
     -- read the world state
     state <- get
     liftIO $ putStrLn (Data.List.intercalate "\n\n" $ startTurn turns state)
     -- Read the action from the user input
     input <- liftIO getAction
     -- If the instruction was understood, do the action, otherwise go again. 
     either (endTurn turns) (doAction turns) input

startTurn :: TurnsLeft -> (World, PlayerState) -> [String]
startTurn turns (w, player) =
   let current = Player.getRoom player
       description = getDetail (getRooms w) current
       exits = getExits (getTransitions w) current
       roomItems = describeRoomItems w current
   in  ["----------------------------------------------------------"] ++
       description ++ [ roomItems ] ++
       [ (describeExits w current) ] ++
       ["\nYou have " ++ (show turns) ++ " turn(s) remaining. What is your move?\n" ]


pickupItem :: ItemInfo -> App ()
pickupItem item@(ItemInfo itemId _ _ _) = do
     (world, player) <- get
     let newState = runUpdate (world, player) (pickupUpdates item)
     put newState


dropItem :: ItemInfo -> App ()
dropItem item@(ItemInfo itemId _ _ _) = do
     (world, player) <- get
     let newState = runUpdate (world, player) (dropUpdates item (Player.getRoom player))
     -- update the world items so that the item info is in the current room
     put newState


useItem :: ItemInfo -> App()
useItem item@(ItemInfo itemId _ _ _) = do
        (world, player) <- get
        liftIO $ putStrLn ("Not implemented yet. Sorry.")
        put (world, player)


itemNotThere :: String -> App ()
itemNotThere name = do
      let message = "The item: " ++ name ++ " is not here."
      tell message
      liftIO $ putStrLn $ message
 
itemNotInInventory :: String -> App ()
itemNotInInventory name = do
      let message = "The item: " ++ name ++ " is not in the inventory."
      tell message
      liftIO $ putStrLn $ message

