module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer p = "Player " ++ show p ++ "'s turn: enter a row and column position (ex. A1)"

promptPlayer' :: Player -> String
promptPlayer' p = concat ["Player ", show p, "'s turn: enter a row and column position (ex"]

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit x 
   | isDigit x  = read [x]
   | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: [[Square]]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
        [X, O, O]
      , [O, X, X]
      , [O, X, O]
      ]

isTied :: Board -> Bool
isTied b 
   | elem True $ concatMap (map (== Empty)) b = False
   | otherwise                                 = True

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings xs = zip ['A'..] xs

-- or

indexRowStrings' :: [String] -> [(Char, String)]
indexRowStrings' xs = [(x,y) | x <- ['A'..], y <- xs]

-- Q#07

formatLine :: [String] -> String
formatLine xs = intercalate _SEP_ xs

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = 
   let check  = x >= 0 && x <= 2
       check' = y >= 0 && y <= 2
   in check && check'

isMoveInBounds' :: Move -> Bool
isMoveInBounds' (x, y) = (x >= 0 && x <= 2) && (y >= 0 && y <= 2)

-- Q#09

stringToMove :: String -> Move
stringToMove []        = _INVALID_MOVE_
stringToMove [_]       = _INVALID_MOVE_
stringToMove (_:_:_:_) = _INVALID_MOVE_
stringToMove [x, y] = (convertRowIndex x , readDigit y) 

-- Q#10

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O 

e = head _EMPTY_BOARD_ 
t = last _TIED_BOARD_

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []
replaceSquareInRow a i xs 
             | i == 0 = (\(ys,zs)-> a : zs) $ splitAt (i+1) xs
             | i == 1 = (\(ys,zs)-> init ys ++ [a] ++ zs) $ splitAt (i+1) xs
             | i == 2 = init xs ++ [a]
             | otherwise = xs




