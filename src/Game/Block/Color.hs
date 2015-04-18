module Game.Block.Color where

import Game.IO
import Game.Random
import System.Random

import Debug.Trace

data Color = C1 | C2 | C3 | C4 deriving (Eq, Show, Enum)

instance Parsable Color where
    parse "1" = C1
    parse "2" = C2
    parse "3" = C3
    parse "4" = C4
    
instance Pretty Color where
    pretty C1 = "1"
    pretty C2 = "2"
    pretty C3 = "3"
    pretty C4 = "4"

randomColor :: (RandomGen g) => g -> (Color,g)
randomColor = random

instance Random Color where
    randomR (lo,hi) g = (toEnum i, g')
        where
            lo' = fromEnum lo
            hi' = fromEnum hi
            (i,g') = randomR (lo',hi') g
            
    random = randomR (C1,C4)
