{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module GameCollection where

{-
what exactly am I trying to generalise?

 1. ItemCollection -> Item -> Maybe ItemInfo
 2. Transitions -> (Room, Direction) -> Maybe Room
 3. RoomCollection -> Room -> Maybe RoomInfo
 4. UsageCollection -> (Room, Item) -> Maybe WorldChanger
 5. VerbCollection -> UserCommand -> Maybe Command

 General form is: Collection k v =
-}

class GameCollection c k v where
  lookin :: c -> k -> Maybe v




  -- class GameCollection c k v => MapGameCollection k v where
  --   lookup :: Map k v -> k -> Maybe v
  --
  -- instance MapGameCollection where
  --   definitions
-- instance GameCollection ItemCollection AKey AValue where
--   lookin :: ItemCollection -> AKey -> Maybe AValue
--   lookin c k = Data.Map.lookup k c
