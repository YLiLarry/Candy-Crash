module Main where
    
import Game
import Game.IO
import System.Random
import Control.Monad
import Control
import Game.Board.IArray
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.State

interactState :: s -> (String -> s -> Writer String s) -> IO s
interactState state f = do
    input <- getLine
    if input == "q" then return state 
    else do
        let (state',output) = runWriter $ f input state
        putStr output
        interactState state' f
    

main = do
    b <- return $ (parse "1__ 1__ 1__\n1__ 1__ 1__\n1__ 1__ 1__\n" :: Board)
    putStr $ pretty b
    interactState (b, mkStdGen 1) command

