module Actions where

import Direction
import Data.Char

data Action = Move Direction | Inventory deriving (Eq, Show)

findAction :: String -> Either String Action
findAction "N" = Right (Move North)
findAction "S" = Right (Move South)
findAction "E" = Right (Move East)
findAction "W" = Right (Move West)
findAction "LIST" = Right Inventory
findAction msg = Left msg

getAction :: IO (Either String Action)
getAction = fmap (findAction . map toUpper) getLine
   
