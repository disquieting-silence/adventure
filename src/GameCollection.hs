{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module GameCollection where

import Data.Map
import Data.List
{-
what exactly am I trying to generalise?

 1. ItemCollection -> Item -> Maybe ItemInfo
 2. Transitions -> (Room, Direction) -> Maybe Room
 3. RoomCollection -> Room -> Maybe RoomInfo
 4. UsageCollection -> (Room, Item) -> Maybe WorldChanger
 5. VerbCollection -> UserCommand -> Maybe Command

 So:
  Items are Map Item ItemInfo  [ Grouping ]
  Transitions are Map (Room, Direction) Room [ Outcome ]
  Room are Map Room RoomInfo [ Grouping ]
  Usage are Map (Room, Item) (WorldF, StateT) [ Outcome ]
  Verb are Collection Synonym Command [ Outcome ]


 General form is: Collection k v =
-}

{-
Start small ... I want to create something which allows me to
find something in a list of things where that thing has a mapping
between one thing and another. I honestly think the only way I'll be
able to do it is if I use a MultiParamTypeClasses extension

Let's try that again. This time, keep the signatures to only the things
that require both types ... use constraints for the types and put the signatures
in those constraining types



-}

class Eq k => GameId k where
  idToStrings :: k -> [ String ]

class GameInfo i where
  infoToStrings :: i -> [ String ]

class (GameId k, GameInfo i) => GameGroup i k | i -> k where
  toId :: i -> k

class (GameId k, GameInfo i) => GameCollection c i k | c -> i where
  findFromId :: c -> k -> Maybe i

-- instance (Ord k, GameId k, GameInfo i, GameGroup i k) => GameCollection (Map k i) i k where
--   findFromId c k = Data.Map.lookup k c

-- Not necessary, but good to keep around to remind me how it works.
-- instance (GameId k, GameInfo i, GameGroup i k) => GameCollection [i] i k where
--   findFromId :: [i] -> k -> Maybe i
--   findFromId c k = Data.List.find (\info -> (toId info) == k) c


getDetail :: (GameCollection c i k) => c -> k -> [String]
getDetail c k =
  let info = findFromId c k
  in maybe ["I do not ."] infoToStrings info
