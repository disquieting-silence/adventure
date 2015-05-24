module Actions where

import Direction

data Action = Move Direction deriving (Eq, Show)

findAction :: String -> Either String Action
findAction "N" = Right (Move North)
findAction "S" = Right (Move South)
findAction "E" = Right (Move East)
findAction "W" = Right (Move West)
findAction msg = Left msg

getAction :: IO (Either String Action)
getAction = fmap findAction getLine
   
