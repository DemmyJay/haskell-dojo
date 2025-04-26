{-# Language RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

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

-- There's something wrong with this code, and whey this code wouldn't work
countDown :: Int -> String
countDown 0 = "Liftoff!" 
countDown n =
    if n > 0
    then countDown (n - 1)     
    else "Failure to Launch"

-- Understand how tuples work
check :: (Num a) => (a, String)
check = (2, "string") 

zip' :: [a] -> [b] -> [(a,b)]
zip'  _ []  = []
zip'  [] _  = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

test = ['A'.. (toEnum (64 + (length [])))] 


----------------------------- Advanced Haskell Functionalities -------------------------

-- Using {-# Language RankNTypes #-} extension
fun :: Num b => (forall a. Show a => a -> [a]) -> Int -> [b]
fun f i = undefined

test' = fun -- test' is more of a container here which is defined as the function "fun"

{-
1.
type family Add a b where
  Add Int Int = Int
  Add String String = String

add :: Add a b -> a -> b -> Add a b
add _ x y = x + y

2.
data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  Equal   :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (LitInt n)      = n
eval (LitBool b)     = b
eval (Add e1 e2)     = eval e1 + eval e2
eval (Equal e1 e2)   = eval e1 == eval e2

3.
 -- TYPE LEVEL PROGRAMMING:
   -- I would need deeper understanding about this:

type family (a :: Nat) + (b :: Nat) :: Nat where
  'Zero + b = b
  'Succ a + b = 'Succ (a + b)

4.

-}


data Omni = Profile { name    :: String ,
                      country :: String ,
                      role    :: String
                  } deriving Show

demmy :: Omni
demmy = Profile { name    = "Demilade",
                  country = "Nigeria",
                  role    = "Developer"

}
demmy' :: Omni
demmy' = demmy {role = "software architect"}










