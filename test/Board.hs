import Test.HUnit.Base
import Test.HUnit.Text
import Control.Monad
import Game.Board.IArray
import Data.Array.IArray (listArray)
import System.Exit
import Prelude hiding (Right)

createBoard size ls = create size ls :: Board

unitTests :: Test 
unitTests = TestList [
        
        TestLabel "inBoard" $ TestList [
            inBoard (1,1) (createBoard (1,1) []) ~?= True,
            inBoard (1,9) (createBoard (5,5) []) ~?= False
        ],
        
        TestLabel "allInBoard" $ TestList [
            allInBoard [(1,9),(2,2)] (createBoard (5,5) []) ~?= False,
            allInBoard [(1,5),(2,2)] (createBoard (5,5) []) ~?= True
        ],
        
        TestLabel "allCords" $ TestList [
            allCords (createBoard (2,2) []) ~?= [(1,1),(1,2),(2,1),(2,2)]
        ],
        
        TestLabel "clear" $ TestList [
            let a = Block (Just C1) False Nothing in
            let b = Block (Just C1) True Nothing in
            (clear [ (x,y) | x <- [1..3], y <- [1..3] ] (createBoard (3,3) $ repeat a))
            ~?= 
            (createBoard (3,3) $ repeat b)
        ],
        
        TestLabel "colorAt" $ TestList [
            let a = Block (Just C1) False Nothing in
            (colorAt (1,1) (createBoard (3,3) $ repeat a))
            ~?= 
            (Just C1)
        ],
        
        let a = Block (Just C1) False Nothing in
        let b = Block (Just C2) False Nothing in
        TestLabel "sameColor" $ TestList [
            (sameColor [ (x,y) | x <- [1..3], y <- [1..3] ] $ createBoard (3,3) $ repeat a)
            ~?= 
            True,
            (sameColor [ (x,y) | x <- [1..3], y <- [1..3] ] $ createBoard (3,3) $ cycle [a,b])
            ~?= 
            False
        ],
        
        let a     = Block (Just C1) False Nothing in
        let board = createBoard (5,5) $ repeat a in
        TestLabel "setBonusAt" $ TestList [
            TestCase $
                (setBonusAt (1,1) v3 board) @?= (board),
            TestCase $
                (setBonusAt (1,1) h3 board) @?= (board),
            TestCase $
                (setBonusAt (2,2) v4 board) @?= (board // [((3,2), Block (Just C1) False (Just Vertical))]),
            TestCase $
                (setBonusAt (2,2) h4 board) @?= (board // [((2,3), Block (Just C1) False (Just Horizontal))]),
            TestCase $
                (setBonusAt (2,2) s3 board) @?= (board // [((3,3), Block (Just C1) False (Just Square))])
        ],
        
        TestLabel "swap" $ TestList [
            TestCase $
                (swap (0,0) Down $ parse "1__ 2r_ \n 3_v 4rs" :: Board)
                @?=
                (parse "3_v 2r_ \n 1__ 4rs" :: Board)
          , TestCase $
                (swap (1,0) Right $ parse "1__ 2r_ \n 3_v 4rs" :: Board)
                @?=
                (parse "1__ 2r_ \n 4rs 3_v" :: Board)
        ]
        
    ]

main = do
    x <- runTestTT $ unitTests
    if (failures x > 0) then exitFailure else exitSuccess

