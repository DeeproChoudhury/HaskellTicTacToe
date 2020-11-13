module Tests (Tests.main) where
import IC.TestSuite
import TicTacToe hiding (main)


gameOverTestCases
   = [ testBoard1 ==> True
     , testBoard2 ==> False
     , testBoard3 ==> True
     ]

parsePositionTestCases
   = [
       ("0 2") ==> (Just (0,2))
     , ("0 -8") ==> (Just (0,-8))
     , ("-4 1") ==> (Just (-4,1))
     , ("0 %1") ==> (Nothing)
     , ("") ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     ]

tryMoveTestCases
  = [
      (X,(0,0),testBoard2) ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2) ==> (Just ([Taken X,Empty,Empty,Taken O],2))
    , (O,(3,3),testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken O,
                                Empty,Taken X,Taken X,Taken O,Empty,Empty,Taken
                                X,Taken O,Taken X,Empty,Taken O],4))
    --Added by me to test positions out of bounds above limits and testBoard3
    , (X,(32, 32),testBoard3) ==> (Nothing)
    ]

--Added by me to test enhanced gameOver' function
gameOver'TestCases
  = [
      ([Taken O, Taken X, Taken O, Taken O, Taken X, 
      Taken O, Taken X, Taken O, Taken X], 3) 
       ==> (Just "Draw")
    , ([Taken X, Taken X, Taken X, Empty, Empty, Empty,
       Empty, Empty, Empty], 3)
       ==> (Just "Win X")
    , ([Taken O, Taken O, Taken O, Empty, Empty, Empty,
       Taken X, Taken X, Empty], 3)
       ==> (Just "Win O")
    , ([Empty, Empty, Empty, Empty, Empty, Empty, Empty,
       Empty, Empty], 3)
       ==> (Nothing)
  ] 

-- You can add your own test cases above

allTestCases
  = [
      TestCase "gameOver" (gameOver)
               gameOverTestCases
    , TestCase "parsePosition" (parsePosition)
               parsePositionTestCases
    , TestCase "tryMove" (uncurry3 tryMove)
               tryMoveTestCases
    , TestCase "gameOver'" (gameOver')
               gameOver'TestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
