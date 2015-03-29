module Game.Block.Color where

import Game.IO

data Color = C1 | C2 | C3 | C4 deriving (Eq, Show)

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
