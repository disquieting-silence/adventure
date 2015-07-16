module Actions where

import Direction
import Data.Char
import Data.List

data Action = Move Direction | Inventory | Pickup String | Drop String | Use String | Quit deriving (Eq, Show)

-- I need to create an association list of [ terms ] - builder

type AdvancedList = [([String], String -> Action)]
advanced :: AdvancedList
advanced = [ (pickupVerbs, Pickup), (dropVerbs, Drop), (useVerbs, Use) ]

findAdvanced :: String -> AdvancedList -> Maybe Action 
findAdvanced input adv =
  foldr (\(vs, f) b -> 
    let command = find (\c -> isPrefixOf c input) vs
    in maybe b (\c -> Just $ f (drop (length c + 1) input)) command
  ) Nothing adv
    


findAction :: String -> Either String Action
findAction "N" = Right (Move North)
findAction "S" = Right (Move South)
findAction "E" = Right (Move East)
findAction "W" = Right (Move West)
findAction "LIST" = Right Inventory
findAction "Q" = Right Quit
findAction action =
         maybe (Left action) Right (findAdvanced action advanced)


pickupVerbs = [ "TAKE", "PICK UP" ]

dropVerbs = [ "DROP", "PUT DOWN" ]

useVerbs = [ "USE" ]

getAction :: IO (Either String Action)
getAction = fmap (findAction . map toUpper) getLine
   
