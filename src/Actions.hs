module Actions where

import Direction
import Data.Char
import Data.List

data Action = Move Direction | Inventory | Pickup String | Quit deriving (Eq, Show)

findAction :: String -> Either String Action
findAction "N" = Right (Move North)
findAction "S" = Right (Move South)
findAction "E" = Right (Move East)
findAction "W" = Right (Move West)
findAction "LIST" = Right Inventory
findAction "Q" = Right Quit
findAction action =
	let optVerb = find (\verb -> isPrefixOf verb action) pickupVerbs
        in maybe (Left action) (\v -> Right $ Pickup $ drop (length v + 1) action) optVerb

pickupVerbs = [ "TAKE", "PICK UP" ]


getAction :: IO (Either String Action)
getAction = fmap (findAction . map toUpper) getLine
   
