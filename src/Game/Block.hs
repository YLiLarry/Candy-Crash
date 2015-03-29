module Game.Block (
    module Game.Block
  , module Game.Block.Color
) where
    
import Game.IO
import Game.Match
import Game.Block.Color

data Block = Block { 
    blockColor   :: Maybe Color, 
    blockRemoved :: Bool, 
    blockEffect  :: Maybe Effect
} deriving (Eq, Show)

instance PrettyShow Block where
    prettyShow (Block c r e) = show c ++ show r ++ show e

instance Parsable Block where
    parse str = Block Nothing True Nothing 
