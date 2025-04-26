module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x : xs) =  show x : showInts xs

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares :: Row -> [String]
showSquares [] = []
showSquares (x : xs) = show x : showSquares xs 

-- Q#03

formatRows :: Board -> [String]
formatRows xs = map (formatLine . showSquares) xs

-- Q#04

isColEmpty = undefined

-- Q#05

dropFirstCol = undefined

dropLastCol = undefined

-- Q#06

getDiag1 = undefined

getDiag2 = undefined

getAllLines = undefined

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined