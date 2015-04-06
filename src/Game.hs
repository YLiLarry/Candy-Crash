module Game where

import Game.Board.IArray
import System.Random
    
gameStart:: (RandomGen g) => g -> Int -> Board
gameStart g size = create (size,size) $ randomBlocks g
    where
        randomBlocks g = let (b,g') = randomBlock g in b : randomBlocks g' 
