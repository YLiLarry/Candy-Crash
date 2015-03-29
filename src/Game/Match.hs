module Game.Match (
    module Game.Match
  , module Game.Match.Effect
) where

import Game.IO
import Game.Board.Cord
import Game.Match.Effect

data Match = Match {
    matchArea   :: Cord -> [Cord], 
    matchCenter :: Cord -> Cord, 
    matchEffect :: Maybe Effect
}

v3 = Match {
    matchArea   = \(x,y)->[(x+a,y)|a<-[0..2]],
    matchCenter = id,
    matchEffect = Nothing
}

h3 = Match {
    matchArea   = \(x,y)->[(x,y+a)|a<-[0..2]],
    matchCenter = id,
    matchEffect = Nothing
}

v4 = Match {
    matchArea   = \(x,y)->[(x+a,y)|a<-[0..3]],
    matchCenter = \(x,y)->(x+1,y),
    matchEffect = Just Vertical
}

h4 = Match {
    matchArea   = \(x,y)->[(x,y+a)|a<-[0..3]],
    matchCenter = \(x,y)->(x,y+1),
    matchEffect = Just Horizontal 
}

s3 = Match {
    matchArea   = \(x,y)->[(x+a,y+b)|a<-[0..2],b<-[0..2]],
    matchCenter = \(x,y)->(x+1,y+1),
    matchEffect = Just Square
}

allMatchTypes = [s3,v4,h4,v3,h3]
