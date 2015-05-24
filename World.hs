module World where

import Data.List
import Control.Monad.State
import Control.Monad.Writer

data Room = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Show, Eq)

newtype RoomDesc = RoomDesc String deriving Show

data RoomInfo = RoomInfo {
  getRoom :: Room,
  getDesc :: RoomDesc
} deriving Show

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


data Action = Move Direction deriving (Eq, Show)

findAction :: String -> Either String Action
findAction "N" = Right (Move North)
findAction "S" = Right (Move South)
findAction "E" = Right (Move East)
findAction "W" = Right (Move West)
findAction msg = Left msg

getAction :: IO (Either String Action)
getAction = fmap findAction getLine
   
    

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

type App a = WriterT String (StateT (World, PlayerState) IO) a

type TurnsLeft = Int

doAction :: TurnsLeft -> Action -> App GameOutcome
doAction turns (Move dir) = do
     state <- get
     let (message, world, ps) = doMove state dir
     tell $ message ++ "\n"
     put (world, ps)
     playGame (turns - 1)


endTurn :: TurnsLeft -> String -> App GameOutcome
endTurn turns msg = do
     liftIO $ putStrLn ("I did not understand your command: \"" ++ msg ++ "\"")
     playGame turns 


findInfo :: World -> Room -> Maybe RoomInfo
findInfo w room = 
     let rooms = getRooms w
     in find (\(RoomInfo r d) -> r == room) rooms

startTurn :: TurnsLeft -> (World, PlayerState) -> [String]
startTurn turns (w, (PlayerState current)) = 
   let info = findInfo w current
       description = maybe 
        [ "I do not know where you are." ]
        (\i -> 
          let (RoomDesc desc) = getDesc i
          in [ "You are in the " ++ desc ]
        )
        info
   in [ "You have " ++ (show turns) ++ " turn(s) remaining." ] ++ description ++ [ "What is your move?" ]


playGame :: TurnsLeft -> App GameOutcome 
playGame 0 = return Lose
playGame turns = do
     -- read the world state
     state <- get
     liftIO $ putStrLn (Data.List.intercalate "\n" $ startTurn turns state)
     -- Read the action from the user input
     input <- liftIO getAction
     -- If the instruction was understood, do the action, otherwise go again. 
     either (endTurn turns) (doAction turns) input


runGame :: TurnsLeft -> IO ()
runGame turns = do 
  (runStateT $ runWriterT (playGame turns)) (gameWorld, PlayerState R1)
  return ()

-- this is sort of running something.
-- (runStateT $ runWriterT (playGame [South, South])) (gameWorld, (PlayerState R1))

-- |
-- Testing moving south from R1
-- >>> move gameWorld R1 South
-- Just R2
