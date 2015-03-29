module Game.Board.IArray (
    module Game.Board.IArray
  , module Game.Board
) where

import Game.Board
import Data.Array.IArray as IA 

newtype Board = Board (Array Cord Block) deriving (Show, Eq)
instance BoardLike Board where
    set ls (Board b)   = Board $ (IA.//) b ls
    get cord (Board b) = (IA.!) b cord
    create lim         = Board . listArray ((1,1),lim)
    size (Board b)     = let (_,lim) = bounds b in lim

--instance PrettyShow Board
--instance Functor Board
--instance Parsable Board
