module Game.Block (
    module Game.Block
  , module Game.Block.Color
) where

import Game.IO
import Game.Match
import Game.Block.Color
import Game.Random

data Block = Block { 
    blockColor   :: Color, 
    blockRemoved :: Bool, 
    blockEffect  :: Maybe Effect
} deriving (Eq, Show)

instance Pretty Block where
    pretty (Block c r e) = pretty c ++ pretty r ++ pretty e

instance Parsable Block where
    parse [color, removed, effect] = Block (parseC color) (parseC removed) (parseC effect)

instance Parsable Bool where
    parse "r" = True
    parse _   = False

instance Pretty Bool where
    pretty True  = "r"
    pretty False = "_"
    
randomBlock :: (RandomGen g) => g -> (Block,g)
randomBlock g = (b,g') 
    where
        b = Block {
                blockColor   = c,
                blockRemoved = False,
                blockEffect  = Nothing
            }
        (c,g') = randomColor g
        
randomBlocks :: (RandomGen g) => g -> [Block]
randomBlocks = randoms 

instance Random Block where
    randomR = undefined
    random  = randomBlock
