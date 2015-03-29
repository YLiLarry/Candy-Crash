module Main where
    
import Game.Board.IArray

main = do
    b <- return (parse "1_h 2r_\n 3_v 4_s\n" :: Board)
    putStr $ pretty b
