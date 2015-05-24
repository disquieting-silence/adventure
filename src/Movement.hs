module Movement where

import Direction
import Rooms

data Transition = Transition Room Direction Room deriving Show
