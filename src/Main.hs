module Main where
    
import Game
import Game.IO
import System.Random
import Control.Monad
import Control
import Game.Board.IArray

main = do
    b <- return $ (parse "1f_ 1f_ 1r_\n1r_ 1r_ 1r_\n1r_ 1r_ 1r_\n" :: Board)
    putStr $ pretty b
    interact $ unlines . map (pretty . fst . (flip $ command $ mkStdGen 1) b) . lines
    