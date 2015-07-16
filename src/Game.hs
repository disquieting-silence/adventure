module Game where

data GameOutcome = Win | Lose deriving Show

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
     let specItem = getItemInInventory world player obj
     _ <- maybe (itemNotInInventory obj) dropItem specItem
     playGame (turns - 1)
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



