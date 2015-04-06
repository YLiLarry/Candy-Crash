module Game.Board.IArray (
    module Game.Board.IArray
  , module Game.Board
) where

import Game.Board
import Data.Array.IArray as IA 
import Control

newtype Board = Board (Array Cord Block) deriving (Show, Eq)
instance BoardLike Board where
    set ls (Board b)   = Board $ (IA.//) b ls
    get cord (Board b) = b IA.! cord
    create lim         = Board . listArray ((1,1),lim)
    size (Board b)     = let (_,lim) = bounds b in lim

instance Pretty Board where
    pretty = showBoard

instance Parsable Board where
    parse = readBoard

instance Control Board
