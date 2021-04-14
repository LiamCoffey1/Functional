{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Language.Haskell.TH
-- Arithmatics

-- 2 + 15
-- 17

-- (* / - +)
-- () - precedence

-- Boolean algebra
-- Regular && and ||
-- not False = True
-- Equality as normal /= (not equal)


-- Functions
-- min 9 10 = 9
-- max 100 101 = 101
-- succ 8 = 9

add5 :: Integer -> Integer
add5 a = a + 5

sumOf :: Integer -> Integer -> Integer
sumOf a b = a + b

doubleSmallNumber :: Integer -> Integer
doubleSmallNumber x = if x > 100
                       then x
                       else x*2

--String creation ("string")

-- Lists - homeogenous data structure (eles same type)
-- denoted by []

-- Concatanation ++
-- [1,2,3,4] ++ [5,6,7] = [1,2,3,4,5,6,7]
-- Same with strings (list of chars)
-- [] - empty list
-- [[]] - list that contains empty list
-- Get element - !! {array} !! {index > n}
-- Can be compared in lexicographical order
-- first heads are compared, if equal pair
-- < <= > >=


-- list functions:
-- head {array} - get first ele
-- tail {array} - chop head off
-- last {array} - get last ele
-- init {array} - chop tail off
-- length {array} - get length
-- null {array} - check if empty set
-- reverse {array}
-- take {n} {array} - get first n eles
-- drop {n} {array} -- get last n eles
-- minimum {array}
-- maximum {{array}
-- sum
-- product
-- {n} 'elem' {array} - is n in array
-- [1..20] - 1 to 20
-- ['a'..'z'] - lowercase alphabet
-- [2,4..20] - up in twos (must define first two eles)
-- replicate 3 10 = [10,10,10]

-- list comprehensions
-- [x*2 | x <- [1..10]]
-- = [2,4,6,8, .. 20]
-- predicate
-- [x*2 | x <- [1..10], x*2 >= 12]
-- [12,14,16,18,20]
-- weeding out lists by predicates - filtering
-- filtering

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--boomBangs [7..13]
--["BOOM!","BOOM!", "BANG!","BANG!"]

-- Multiple predicates
-- [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
-- [10,11,12,14,16,17,18,20]

--Tuple - (A,B) - free from homogenous
-- [(A1,B2), (A2,B2), (A3,B3)]
-- fst (8,10) = 8
-- snd (8,11) = 11
-- only works on pairs ¬
-- zip - array to array of pairs
-- zip [1,2,3,4,5] [5,5,5,5,5]
-- [(1,5), (2,5), (3,5), (4,5), (5,5)]



-- :t {Anything} - get type of anything

removeNonUppercase :: [Char] -> [Char]  -- Explicit type declaration
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   -- function

-- Types
-- Int, Integer,Float,Double,Bool,Char
-- => class constraint
-- Eq - equality testing, members implement == and /=
-- Ord - either GT, LT, Or EQ
-- Show - can be presented as strings
-- Read - opisite of show
-- Bounded - have an upper and lower bound
-- Enum -- can use types in list ranges
-- Num - acts like number
-- Integral - whole numbers
-- Floating - decimal


-- Syntax in Functions

-- All functions curried (one param at a time)


-- Pattern matching
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!" -- else case


-- Recursion with pattern matching
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  


-- '_' in this case means anything(dont care)
first :: (a, b, c) -> a  
first (x, _, _) = x        
second :: (a, b, c) -> b
second (_, y, _) = y        
third :: (a, b, c) -> c
third (_, _, z) = z

-- note: can only be done this way when defining functions


-- Patern matching with |
-- note otherwise
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft,!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"


--Sugar for repeating var
-- where bindings
-- arent shares across boies of different patterns

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2 -- Sugar

where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2



-- Let - bind variables at end of functions and whole
-- function can see
-- let <bindings in <expression>
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea


-- Case expressions
--  case expression of pattern -> result  
--                     pattern -> result  
--                     pattern -> result  
--                     ...

-- can be used anywhere
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                                [x] -> "a singleton list."   
                                                xs -> "a longer list."


--Recursion

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  


reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]  
repeat' x = x:repeat' x



zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  
