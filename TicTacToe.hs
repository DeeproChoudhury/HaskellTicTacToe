module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read
import System.IO

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)


-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------
p :: [Cell] -> Bool
p q 
  = case nub q of 
      [x] -> x /= Empty
      _   -> False

gameOver :: Board -> Bool
gameOver n
   = or (map (any p) [rows n, cols n, diags n])

--Checks to see who won
gameOver' :: Board -> Maybe String
gameOver' b
  | elem [Taken X] e            = Just "Win X"
  | elem [Taken O] e            = Just "Win O"
  | filter (elem Empty) e /= [] = Nothing
  | otherwise                   = Just "Draw"
  where
    e = map nub $ concatMap ($ b) [diags, rows, cols]


-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition s
  = case (words s) of
      [p, q] -> do 
        x <- readMaybe p
        y <- readMaybe q
        return (x, y)
      _    -> Nothing

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove m p b
  | (inbounds p) && (checkCell (fst b !! index)) = Just (replace index (Taken m) (fst b), snd b)
  | otherwise = Nothing
  where
    index = (fst p) * (snd b) + (snd p)  
    inbounds (i, j)
      = (0 <= i && i < snd b) && (0 <= j && j < snd b)
    checkCell Empty = True
    checkCell _     = False

swapPlayer :: Player -> Player
swapPlayer X = O 
swapPlayer O = X


-------------------------------------------------------------------
-- I/O Functions

-- Flush the output buffer after printing without newline char
-- to fix the behaviour when program compiled
putStrFlush :: String -> IO ()
putStrFlush text = do
    putStr text
    hFlush stdout

prettyPrint :: Board -> IO ()
prettyPrint b
  = mapM_ prettyPrint' rs
    where
      rs = rows b
      prettyPrint' :: [Cell] -> IO ()
      prettyPrint' cells
        = do
            putStrLn (intersperse ' ' (concatMap printCell cells))

doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction f errorMsg
  = do
      str <- getLine
      let parsed = f str
      case parsed of  (Just val) -> return val
                      Nothing    -> do
                                      putStrFlush errorMsg
                                      doParseAction f errorMsg



-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b pl
  = do
      putStrFlush ("Player " ++ (show pl) ++ " make your move (row col): ")
      doParseAction (\x -> do position <- parsePosition x
                              tryMove pl position b) "Invalid move, try again: "

     

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b pl
  = do
      --system "cls"
      case gameOver' b of
        Just "Win X" -> putStrLn "Player X wins!"
        Just "Win O" -> putStrLn "Player O wins!"
        Just "Draw"  -> putStrLn "It's a tie!"
        Nothing      ->
          do
            putStrLn $ "Player:  " ++ show pl
            prettyPrint b
            takeTurn b pl >>= flip playGame (swapPlayer pl)

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to tic tac toe on an N x N board"
      -- Get board size
      putStrFlush "Enter the board size (N > 2): "
      n <- doParseAction getN "Invalid N size, try again: "
      -- Get First Player
      --putStrFlush "Which player should go first? X or O: "
      --p <- doParseAction getFirstPlayer "Please select either X or O: "
      
      -- Play the game
      playGame (replicate (n*n) Empty, n) X
      putStrLn "Thank you for playing"
      
      
printCell :: Cell -> String
printCell Empty = "-"
printCell (Taken x) = show x

getN str
  = filterMaybe (\i -> i > 2) (readMaybe str :: Maybe Int)
      
--getFirstPlayer str
--  = (readMaybe (map toUpper str) :: Maybe Player)
      

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
