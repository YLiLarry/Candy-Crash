module Control where
    
import Game.Board
import Game.IO
import Debug.Trace
import Game.Random
    
class (BoardLike b) => Control b where
    command :: (RandomGen g) => g -> String -> b -> (b,g)
    command g s = traceShow (words s) $ praseCommand g $ words s

    praseCommand :: (RandomGen g) => g -> [String] -> b -> (b,g)
    praseCommand g ["swap",x,y,d] b = (swap (read x,read y) (parse d) b, g)
    praseCommand g ["fill"]       b = fill g b
    praseCommand g _              _ = undefined
    
