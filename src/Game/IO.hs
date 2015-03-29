module Game.IO where

class PrettyShow p where
    prettyShow :: p -> String

class Parsable p where
    parse :: String -> p
    
    ---- pretty format board
    --showBoard :: board -> String
    --showBoard board = unlines [ unwords [ prettyShow $ board ! (a,b) | b <- [1..y] ] | a <- [1..x] ] 
    --    where
    --        (_,(x,y)) = bounds board 

    
--instance Parsable Board where
--    parse str = Board $ listArray ((1,1),(x,y)) $ fmap parse $ words str
--        where
--            ls@(z:_) = map words $ lines str
--            x        = length ls
--            y        = length z
