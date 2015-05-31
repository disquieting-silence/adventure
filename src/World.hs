module World where

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Rooms
import Actions
import Direction
import Movement
import Item
import Player

data GameOutcome = Win | Lose deriving Show

data World = World {
  getRooms :: [RoomInfo],
  getItems :: [ItemInfo],
  getTransitions :: [Transition]
} deriving Show

type App a = WriterT String (StateT (World, PlayerState) IO) a

type TurnsLeft = Int

listItems :: [ItemInfo] -> [Item] -> String
listItems _ [] = "You have nothing in your inventory."
listItems infos items = 
     let found = mapMaybe (Item.findInfo infos) items
         itemDescs = map Item.showItem found
     in Data.List.intercalate "\n -- " $ [ "Inventory: \n" ] ++ itemDescs

doAction :: TurnsLeft -> Action -> App GameOutcome
doAction turns (Move dir) = do
     (world, player) <- get
     let current = Player.getRoom player
         (message, newTransitions, ps) = doMove (getTransitions world) current dir
     tell $ message ++ "\n"
     liftIO $ putStrLn $ "\n" ++ message
     put (World (getRooms world) (World.getItems world) newTransitions, updateRoom player ps)
     playGame (turns - 1)
doAction turns Inventory = do
     (world, player) <- get
     let items = Player.getItems player
     liftIO $ putStrLn $ "\n" ++ (listItems (World.getItems world) items)
     playGame (turns - 1)
doAction turns Quit = playGame 0
doAction turns (Pickup obj) = do
     (world, player) <- get
     let currentItems = itemsInRoom world (Player.getRoom player)
         specItem = getItemByName currentItems obj
     _ <- maybe (itemNotThere obj) (\i -> pickupItem i) specItem
     playGame (turns - 1)


itemNotThere :: String -> App ()
itemNotThere name = do
     let message = "The item: " ++ name ++ " is not here."
     tell message
     liftIO $ putStrLn $ message


pickupItem :: ItemInfo -> App ()
pickupItem item@(ItemInfo itemId _ _ _) = do
     (world, player) <- get
     let newItems = map (\i@(ItemInfo k n _ d) -> if (item == i) then (ItemInfo k n Nothing d) else i) (World.getItems world)
         newWorld = World (getRooms world) newItems (getTransitions world)
         newPlayerItems = itemId : (Player.getItems player)
         newPlayer = PlayerState (Player.getRoom player) newPlayerItems
     put ((World (getRooms world) newItems (getTransitions world)), newPlayer)



endTurn :: TurnsLeft -> String -> App GameOutcome
endTurn turns msg = do
     liftIO $ putStrLn ("I did not understand your command: \"" ++ msg ++ "\"")
     playGame turns 


itemsInRoom :: World -> Room -> [ItemInfo]
itemsInRoom w room =
   let all = World.getItems w
   in filter (\i -> maybe False (\r -> r == room) (Item.getRoom i)) all 


describeRoomItems :: World -> Room -> String
describeRoomItems w room =
   let inRoom = itemsInRoom w room
       inRoomDescs = map Item.showItem inRoom
   in if (null inRoomDescs) then "" else ("You can see: " ++ (Data.List.intercalate ", " inRoomDescs))

describeExits :: World -> Room -> String
describeExits w room = 
   let exits = getExits (getTransitions w) room
   in if (null exits) then "There are no exits." else "There are exits to the: " ++ (Data.List.intercalate ", " (map show exits))

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



-- this is sort of running something.
-- (runStateT $ runWriterT (playGame [South, South])) (gameWorld, (PlayerState R1))

-- |
-- Testing moving south from R1
-- >>> move gameWorld R1 South
-- Just R2
