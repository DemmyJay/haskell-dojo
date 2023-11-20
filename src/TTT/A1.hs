module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex a = fromEnum (toUpper a) - 65

-- Q#04

_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05

_SEP_ :: [Char]
_SEP_ = " | "

-- Q#06

data Square = X | O | Empty deriving (Show, Eq)

{-
instance Eq Square where
    (==) :: Square -> Square -> Bool
    X     == X     = True
    O     == O     = True
    Empty == Empty = True
    X     == O     = False
    X     == Empty = False
    O     == Empty = False
    O     == X     = False
    Empty == X     = False
    Empty == O     = False
    
    (/=) :: Square -> Square -> Bool
    O     /= O     = False
    X     /= X     = False
    Empty /= Empty = False
    X     /= Empty = True
    O     /= Empty = True
    X     /= O     = True
    Empty /= X     = True
    Empty /= O     = True
    O     /= X     = True
-}


-- Q#07

data GameState = InProgress | XWins | OWins | Tie deriving Show

-- Q#08
type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]
type Move   = (Int,Int)

-- Q#

getFirstPlayer :: Bool -> Player
getFirstPlayer p = if p then X else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ p
    | p         = X
    | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of
    InProgress -> "Game is in progress"
    XWins      -> "X wins!"
    OWins      -> "Y wins!"
    Tie        -> "It's a tie!"

-- Q#11

switchPlayer :: Player -> Player
switchPlayer X     = O
switchPlayer O     = X
switchPlayer Empty = Empty

-- Q#12

showSquare :: Square -> String
showSquare X     = "X"
showSquare O     = "O"
showSquare Empty = "_"