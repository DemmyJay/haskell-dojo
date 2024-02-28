module Sandbox where
import Data.List

x = 3 + 4

main = "Hi Demmy!"

main1 = main ++ ", " ++ "Great job!" ++ " Keep it up."

{-
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False    
elem' a (x:xs) = if a == x
                 then True 
                 else elem' a xs
-}

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = case a == x of 
    False -> elem' a xs
    True  -> True


repeat' :: Enum a => a -> [a]
repeat' a = [a,a..]

y = intercalate [3] [[2], [2], [2], [2]]

data Colour = Red | Blue | Black deriving (Show, Eq)

alwaysBlue :: Bool -> [Colour] -> Bool
alwaysBlue _ (s:xs) = s == Blue || alwaysBlue True xs
alwaysBlue bool [] = bool

countDown :: Int -> String
countDown 0 = "Liftoff!" 
countDown n =
    if n > 0
    then countDown (n - 1)     
    else "Failure to Launch"




