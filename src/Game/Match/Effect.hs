module Game.Match.Effect where
    
import Game.IO
import Game.Random

data Effect = Horizontal | Vertical | Square deriving (Eq, Show)

instance Parsable Effect where
    parse "h" = Horizontal
    parse "v" = Vertical
    parse "s" = Square

instance Pretty Effect where
    pretty Horizontal = "h" 
    pretty Vertical   = "v" 
    pretty Square     = "s" 


randomEffect :: (RandomGen g) => g -> (Maybe Effect,g)
randomEffect g = randomEQ g [Nothing, Just Square, Just Horizontal,Just Vertical]
