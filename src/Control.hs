module Control where
    
import Game.Board
import Game.IO
import Debug.Trace
import Game.Random
import Control.Monad.Trans.Writer.Lazy

class (BoardLike b) => Control b where
    command :: (RandomGen g) => String -> (b,g) -> Writer String (b,g)
    command str st = praseCommand (words str) st

    praseCommand :: (RandomGen g) => [String] -> (b,g) -> Writer String (b,g)
    praseCommand ["swap",x,y,d] (b,g) =
            writer ((b,g), "after swap\n")
        >>= apply (swap (read x,read y) (parse d))
        >>= write "after process\n"
        >>= apply process
        >>= write "after fill\n"
        >>= applyG fill
        where
            write :: String -> (b,g) -> Writer String (b,g)
            write str = writer . flip (,) str
            
            passG :: (b -> b) -> ((b,g) -> (b,g))
            passG f = \(b,g) -> (f b,g)
            
            applyG :: (Pretty b) => ((b,g) -> (b,g)) -> (b,g) -> Writer String (b,g)
            applyG f = \x -> let (b,g) = f x in writer ((b,g), pretty b) 
            
            apply :: (Pretty b) => (b -> b) -> (b,g) -> Writer String (b,g)
            apply = applyG . passG
            