module Game.Board (
    module Game.Board
  , module Game.Match
  , module Game.Block
  , module Game.Board.Cord
) where
    
import Game.Match
import Game.Block
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


    -- check if an area is the same color
    sameColor :: [Cord] -> board -> Bool
    sameColor (x:xs) board = all (\a -> f x == f a) xs
        where
            f = flip colorAt board

    -- get the color at the cord
    colorAt :: Cord -> board -> Maybe Color
    colorAt cord board = blockColor $ board ! cord

    -- set a given area to empty
    clear :: [Cord] -> board -> board
    clear cords board = board // map (\c -> (c, f $ board ! c)) cords
        where
            f (Block c _ e) = Block c True e 

    -- check matches at the given cord and return a board where the area round the cord is replaced with a new state
    processAt :: Cord -> board -> board
    processAt cord board = foldr ($) board funs
        where 
            funs = map (\mType -> let  cords = matchArea mType cord in 
                                  if   allInBoard cords board && sameColor cords board
                                  then (clear cords) . (setBonusAt cord mType)
                                  else id) 
                       allMatchTypes

    -- check the whole board for matches and return a board with a new state
    process :: board -> board
    process board = foldr ($) board funs
        where
            funs = map processAt $ allCords board


    -- check if a cord is valid
    inBoard :: Cord -> board -> Bool
    inBoard c board = elem c $ allCords board

    -- check if a set of cords is valid
    allInBoard :: [Cord] -> board -> Bool
    allInBoard cords board = and $ map ((flip inBoard) board) cords

    allCords :: board -> [Cord]
    allCords board = let (x,y) = size board in [ (a,b) | a<-[1..x], b<-[1..y] ]
