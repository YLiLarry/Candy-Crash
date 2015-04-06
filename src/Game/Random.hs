module Game.Random (
      randomEQ
    , module System.Random
) where

import System.Random

-- | Takes in a random generator, and randomly returns an element from a list with the next generator
randomEQ :: (RandomGen g) => g -> [a] -> (a,g)
randomEQ g ls = (ls !! (int `mod` l), nextGen)
    where
        l = length ls
        (int,nextGen) = next g
