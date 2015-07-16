module World where

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

data GameOutcome = Win | Lose deriving Show

data World = World {
  getRooms :: [RoomInfo],
  getItems :: ItemCollection,
  getTransitions :: Transitions
} deriving Show

type App a = WriterT String (StateT (World, PlayerState) IO) a

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
     let items = itemsInInventory world player
         specItem = getItemByName items obj
     _ <- maybe (itemNotInInventory obj) dropItem specItem
     playGame (turns - 1)
doAction turns (Use obj) = do
     liftIO $ putStrLn "Not implemented, yet"
     playGame turns

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


pickupUpdates :: ItemInfo -> StateChangers
pickupUpdates item@(ItemInfo itemId _ _ _) = (changeItemInWorld item pickupItemFromRoom, addItemToPlayer itemId)


--     let newWorld = changeItemInWorld item (drop2ddItemInRoom (Player.getRoom player)) world 
--         newPlayer = dropItemFromPlayer itemId player
dropUpdates :: ItemInfo -> Room -> StateChangers
dropUpdates item@(ItemInfo itemId _ _ _) room = (changeItemInWorld item (dropItemInRoom room), dropItemFromPlayer itemId)


runUpdate :: (World, PlayerState) -> StateChangers -> (World, PlayerState)
runUpdate (world, player) (fw, fp) =
  let newWorld = fw world
      newPlayer = fp player
  in (newWorld, newPlayer)


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


changeItemInWorld :: ItemInfo -> (ItemInfo -> ItemInfo) -> World -> World
changeItemInWorld item f world = 
  let newItems = changeItem (World.getItems world) item f
  in World (getRooms world) newItems (getTransitions world)


endTurn :: TurnsLeft -> String -> App GameOutcome
endTurn turns msg = do
     liftIO $ putStrLn ("I did not understand your command: \"" ++ msg ++ "\"")
     playGame turns 


itemsInRoom :: World -> Room -> ItemCollection
itemsInRoom w room =
   let all = World.getItems w
   in Data.Map.filter (itemInRoom room) all 

itemsInInventory :: World -> PlayerState -> ItemCollection 
itemsInInventory w player =
   let current = Player.getItems player
       allItems = World.getItems w
   in toCollection $ Data.Maybe.mapMaybe (Item.findInfo allItems) current 


describeRoomItems :: World -> Room -> String
describeRoomItems w room =
   let inRoom = itemsInRoom w room
       inRoomDescs = Data.Map.elems $ Data.Map.map Item.showItem inRoom
   in if (null inRoomDescs) then "" else ("You can see: " ++ (Data.List.intercalate ", " inRoomDescs))

describeExits :: World -> Room -> String
describeExits w room = 
   let exits = getExits (getTransitions w) room
   in if (null exits) then "There are no exits." else "There are exits to the: " ++ (Data.List.intercalate ", " (Data.List.map show exits))

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



testWorld = World 
   [
     RoomInfo R1 (RoomName "Kitchen") (RoomDesc "It is a small room with white walls. There is the faint odour of something rotten."),
     RoomInfo R2 (RoomName "Dining Room") (RoomDesc "The table has been prepared for a banquet of some kind."),
     RoomInfo R3 (RoomName "Living Room") (RoomDesc "The couch is in the centre of the room."),
     RoomInfo R4 (RoomName "Ballroom") (RoomDesc "The room looks abandoned. There must not have been many balls for quite some time."),
     RoomInfo R5 (RoomName "Rumpus Room") (RoomDesc "No-one could have any fun here."),
     RoomInfo R6 (RoomName "Foyer") (RoomDesc "There are portraits all over the walls."),
     RoomInfo R7 (RoomName "Greenhouse") (RoomDesc "The plants seem to be dying."),
     RoomInfo R8 (RoomName "Library") (RoomDesc "It would take a lifetime to read all of these books."),
     RoomInfo R9 (RoomName "Study") (RoomDesc "The room is very quiet.") 
   ]
   
   (Item.toCollection [
     ItemInfo ItemKey (ItemName "Key") (Just R1) (ItemDesc "The key is oddly-shaped and blue."),
     ItemInfo ItemCrowbar (ItemName "Crowbar") Nothing (ItemDesc "The crowbar is lean and silver.")
   ])

   (Data.Map.fromList [
     ((R1, South), R2)
   ])

-- this is sort of running something.
-- (runStateT $ runWriterT (playGame [South, South])) (gameWorld, (PlayerState R1))

-- |
-- Testing moving south from R1
-- >>> move (World.getTransitions testWorld) R1 South
-- Just R2

-- |
-- Testing items in inventory
-- >>> itemsInInventory testWorld (PlayerState R1 [ ItemKey])
-- fromList [(ItemKey,ItemInfo {getItem = ItemKey, getName = ItemName "Key", getRoom = Just R1, getDesc = ItemDesc "The key is oddly-shaped and blue."})]

-- |
-- Testing cache and find
-- >>> cacheAndFind (\x -> x == 10) 5 (Nothing, [])
-- (Nothing,[5])

-- |
-- Testing cache and find when it matches
-- >>> cacheAndFind (\x -> x == 10) 10 (Nothing, [])
-- (Just 10,[])

-- |
-- Testing cache and find when it doesn't match
-- >>> cacheAndFind (\x -> x == "dog") "cat" (Nothing, [ "elephant" ])
-- (Nothing,["cat","elephant"])

-- |
-- | Testing dropping item from player
-- >>> dropItemFromPlayer ItemKey (PlayerState R1 [ItemCrowbar])
-- PlayerState {getRoom = R1, getItems = [ItemCrowbar]}

-- | Testing dropping item from player that they actually have
-- >>> dropItemFromPlayer ItemKey (PlayerState R1 [ItemCrowbar, ItemKey])
-- PlayerState {getRoom = R1, getItems = [ItemCrowbar]}

-- | Testing dropping item from player that only has item
-- >>> dropItemFromPlayer ItemKey (PlayerState R2 [ItemKey])
-- PlayerState {getRoom = R2, getItems = []}

-- | Testing dropping item from player that has no items
-- >>> dropItemFromPlayer ItemKey (PlayerState R2 [])
-- PlayerState {getRoom = R2, getItems = []}
