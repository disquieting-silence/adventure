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


data Action = Move Direction deriving (Eq, Show)

findAction :: String -> Maybe Action
findAction "N" = Just (Move North)
findAction "S" = Just (Move South)
findAction "E" = Just (Move East)
findAction "W" = Just (Move West)
findAction _ = Nothing

getAction :: IO (Maybe Action)
getAction = fmap findAction getLine
   
    

gameWorld = World 
   [
     RoomInfo R1 (RoomDesc "Kitchen") 
   ]
   [
     Transition R1 South R2
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

playGame :: TurnsLeft -> App GameOutcome 
playGame 0 = return Lose
playGame turns = do
     -- Read the action from the user input
     input <- liftIO getAction
     -- 
     maybe (playGame (turns - 1)) (doAction turns) input

-- this is sort of running something.
-- (runStateT $ runWriterT (playGame [South, South])) (gameWorld, (PlayerState R1))

-- |
-- Testing moving south from R1
-- >>> move gameWorld R1 South
-- Just R2
