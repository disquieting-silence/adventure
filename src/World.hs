module World where

import Data.List
import Control.Monad.State
import Control.Monad.Writer
import Rooms
import Actions
import Direction

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
