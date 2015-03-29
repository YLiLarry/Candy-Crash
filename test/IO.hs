import Test.HUnit.Base
import Test.HUnit.Text
import Control.Monad
import Game.Board.IArray
import Data.Array.IArray (listArray)

createBoard size ls = create size ls :: Board

unitTests :: Test 
unitTests = TestList [
        
        TestLabel "parse" $ TestList [
            TestCase $ 
                (parse "1_v 2r_\n 3__ 4_s") 
                ~?= 
                Board (array ((1,1),(2,2)) [ ((1,1),Block (Just C1) False (Just Horizontal))
                                           , ((1,2),Block (Just C2) True (Nothing))
                                           , ((2,1),Block (Just C3) False (Just Vertical))
                                           , ((2,2),Block (Just C4) False (Just Square))
                                           ])
        ],
     
        TestLabel "pretty" $ TestList [
            TestCase $
                (pretty $ parse "1_v 2r_\n 3__ 4_s") 
                ~?=
                "1_v 2r_\n 3__ 4_s"
        ]   
        
    ]

main = do
    void.runTestTT $ unitTests
