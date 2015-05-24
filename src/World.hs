module World where

import Data.List
import Control.Monad.State
import Control.Monad.Writer
import Rooms
import Actions
import Direction
import Movement

data PlayerState = PlayerState Room deriving Show

data GameOutcome = Win | Lose deriving Show

data World = World {
  getRooms :: [RoomInfo],
  getTransitions :: [Transition]
} deriving Show

type App a = WriterT String (StateT (World, PlayerState) IO) a

type TurnsLeft = Int

doAction :: TurnsLeft -> Action -> App GameOutcome
doAction turns (Move dir) = do
     (world, (PlayerState current)) <- get
     let (message, newTransitions, ps) = doMove (getTransitions world) current dir
     tell $ message ++ "\n"
     put (World (getRooms world) newTransitions, PlayerState ps)
     playGame (turns - 1)


endTurn :: TurnsLeft -> String -> App GameOutcome
endTurn turns msg = do
     liftIO $ putStrLn ("I did not understand your command: \"" ++ msg ++ "\"")
     playGame turns 



startTurn :: TurnsLeft -> (World, PlayerState) -> [String]
startTurn turns (w, (PlayerState current)) = 
   let description = getDetail (getRooms w) current
   in  [""] ++ description ++ ["\nYou have " ++ (show turns) ++ " turn(s) remaining. What is your move?\n" ]


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
