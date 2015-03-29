module Game.Board (
    module Game.Board
  , module Game.Match
  , module Game.Block
  , module Game.Board.Cord
  , module Game.IO
) where
    
import Game.Match
import Game.Block
import Game.IO
import Game.Board.Cord
import Control.Applicative

class BoardLike board where
    
    create :: (Int,Int) -> [Block] -> board
    
    size :: board -> (Int,Int)
    
    set  :: [(Cord, Block)] -> board -> board
    (//) :: board -> [(Cord, Block)] -> board
    (//) = flip set
    
    get :: Cord -> board -> Block
    (!) :: board -> Cord -> Block
    (!) = flip get
    
    setBonusAt :: Cord -> Match -> board -> board
    setBonusAt cord m board = board // [(center,Block c r $ matchEffect m)]
        where
            center      = matchCenter m cord
            Block c r _ = board ! center
            
    --gameStart:: Int -> board
    --gameStart size = listArray ((1,1),(size,size)) randomBoard
    --    where
    --        randomBoard:: [Block]
    --        randomBoard = []


    -- | Check if an area is the same color
    sameColor :: [Cord] -> board -> Bool
    sameColor (x:xs) board = all (\a -> f x == f a) xs
        where
            f = flip colorAt board

    -- | Get the color at the cord
    colorAt :: Cord -> board -> Maybe Color
    colorAt cord board = blockColor $ board ! cord

    -- | Set a given area to empty
    clear :: [Cord] -> board -> board
    clear cords board = board // map (\c -> (c, f $ board ! c)) cords
        where
            f (Block c _ e) = Block c True e 

    -- | Check matches at the given cord and return a board where the area round the cord is replaced with a new state
    processAt :: Cord -> board -> board
    processAt cord board = foldr ($) board funs
        where 
            funs = map (\mType -> let  cords = matchArea mType cord in 
                                  if   allInBoard cords board && sameColor cords board
                                  then (clear cords) . (setBonusAt cord mType)
                                  else id) 
                       allMatchTypes

    -- | Check the whole board for matches and return a board with a new state
    process :: board -> board
    process board = foldr ($) board funs
        where
            funs = map processAt $ allCords board


    -- | Check if a cord is valid
    inBoard :: Cord -> board -> Bool
    inBoard c board = elem c $ allCords board

    -- | Check if a set of cords is valid
    allInBoard :: [Cord] -> board -> Bool
    allInBoard cords board = and $ map ((flip inBoard) board) cords

    -- | Get a list of all cords of the board
    allCords :: board -> [Cord]
    allCords board = let (x,y) = size board in [ (a,b) | a<-[1..x], b<-[1..y] ]

    -- | Convert a board to a pretty formatted string
    showBoard :: board -> String
    showBoard board = unlines [ unwords [ pretty $ get (a,b) board | b <- [1..y] ] | a <- [1..x] ] 
        where
            (x,y) = size board 

    -- | Parse a string to a square board according to linebreaks.
    -- For example, "1 2\n 3 4\n" is parsed to (create (2,2) [1,2,3,4])
    readBoard :: String -> board
    readBoard str = create (x,y) $ fmap parse $ words str
        where
            ls@(z:_) = map words $ lines str
            x        = length ls
            y        = length z
