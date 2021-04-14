{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Language.Haskell.TH
import Test.QuickCheck

test n = n + 5



homework :: Integer -> String
homework score
    | score >= 70 = "A"
    | score >= 60 = "B"
    | score >= 50 = "C"
    | score >= 40 = "D"
    | otherwise =  "Fail"

array = [1]

concat' array1 array2 = [array1] ++ [array2]


-- Recursion

-- Print hello world 5 times

helloWorld 0 = return ()
helloWorld n = putStrLn "Hello World!" >>  helloWorld (n-1)


-- Replicate each ele in list n times (Recursive)

replicateOne val 0 = []
replicateOne val n = val:replicateOne val (n-1)

replicateN array n
    | null array = [] 
    | otherwise = replicateOne (head array) n ++ replicateN (tail array) n 
                                                

-- Filter array

filterByMax array maxValue
    | null array = []
    | (head array) >= maxValue = filterByMax (tail array) maxValue
    | otherwise = [(head array)] ++ filterByMax (tail array) maxValue

-- Filter by odd index


filterByOddIndex array n
    | null array = []
    | mod n 2 == 0 = filterByOddIndex (tail array) (n + 1)
    | otherwise = [(head array)] ++ filterByOddIndex (tail array) ( n + 1)


-- List comprehension
filterByOddValue arr = [ val | val <- arr, val `mod` 2 == 0]

-- array of N elements
arrayN n
    | n == 0 = []
    | otherwise = [1] ++ arrayN (n-1)


--reverse a list
reverse' arr
    | null arr = []
    | otherwise = reverse' (tail arr) ++ [head arr]

-- Sum off odd eles
sumOfOddEles array = sum (filterByOddValue array)


-- Length of list
listLength arr
    | null arr = 0
    | otherwise =  1 + listLength (tail arr)


-- Abs array
realify n
    | n < 0 =  n - (n*2)
    | otherwise = n
mapToReal arr = map realify arr

tryIt n
  | n  == 4 = []
  | otherwise = [n] ++ tryIt 0


apply fn val = fn val

addAmount n = apply (\x -> x + 5) n


maybeEven x = if even x then Just x else Nothing


-- ---------------------------------------------------------

-- --Define a Haskell function crossMap which takes a binary
-- --function F and two lists XS and YS,and returns a list of
-- --the result of applying F to each possible pair of elements
-- --with the first taken from XS and the second from YS.
-- --(Order of elements in output is unspecified.)
-- --You should include a type signature.

crossMap :: (a -> b -> c) -> [a] -> [b] -> [c]

crossMap f [] ys = []
-- crossMap f (x:xs) ys =
--   map (f x) ys ++ crossMap f xs ys

crossMap f xs ys = concat (map (\x -> map (f x) ys) xs)

-- ---------------------------------------------------------


--Define scatterGather (see Q1) in Haskell, except that it takes an
-- additional first argument which is the value to use when an index
-- it out of range. Be sure to give a type signature.
-- Examples
-- scatterGather '_' [0,1,4,1,1,7,2] "abcde"
-- => "abebb_c"
-- scatterGather 0 [0,1,4,1,1,7,2] [100,101,102]
-- => [100,101,0,101,101,0,102]

--- !! - indexing operation   arr !! k = kth ele of arr
scatterGather other indices values = map findIt indices
  where
    findIt k =
      if(k > length values || k < 0) then other else values !! k


-- Q2: Haskell
--  Define a Haskell function mapSkip which takes a function and a
--  list and returns the result of applying the given function to
--  every other element of the given list, starting with the first
--  element. Be sure to include a type signature.
--  Example:
--  mapSkip (+1000) [1..6]
--  => [1001,2,1003,4,1005,6]

mapSkip array fun n
    | null array = []
    | mod n 2 == 0 =  [fun (head array)] ++ mapSkip (tail array) fun ( n + 1)
    | otherwise = [(head array)] ++ mapSkip (tail array) fun ( n + 1)

-- Q2: Haskell
--  Define a Haskell function tear, including its type, which takes a
--  predicate and a list and returns a list of two lists, the first
--  those elements of the input list which pass the predicate, the
--  second those that don't, in order.
--  Examples:
--  tear (>5) [1,10,2,12,3,13]
--  => [[10,12,13],[1,2,3]]

tear cond list = [(filter cond list)] ++ [(filterNot cond list)]

filterNot cond list
    | null list = []
    | cond (head list) = filterNot cond (tail list)
    | otherwise = [(head list)] ++ filterNot cond (tail list)



-- Q2: Haskell [25 marks]
-- Define a Haskell function foo, including a type signature, that takes two lists and yields
-- a list combining all the elements in the two input lists, taking 1 from the first list, 2 from
-- the second list, 3 from the first list, 4 from the second list, etc,until both are exhausted.
-- Examples:
-- foo [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18]
--  => [1,11,12,2,3,4,13,14,15,16,5,6,7,8,17,18]
foo l1 l2 = bar 1 l1 l2
bar n l1 l2
    | null l1 && null l2 = []
    | otherwise = take n l1 ++ take (n+1) l2 ++ bar (n+2) (drop n l1) (drop (n+1) l2)


-- Haskell
-- Define a Haskell function weaveHunks which takes an int and
-- two lists and weaves them together in hunks of the given size.
-- Be sure to declare its type signature.
-- Examples:
--  weaveHunks 3 "abcdefghijklmno" "ABCDEFGHIJKLMNO"
--  => "abcABCdefDEFghiGHIjklJKLmnoMNO"
--  weaveHunks 2 [1..10] [11..20]
--  => [1,2,11,12,3,4,13,14,5,6,15,16,7,8,17,18,9,10,19,20]

weaveHunks n l1 l2
    | null l1 && null l2 = []
    | otherwise = take n l1 ++ take n l2 ++ weaveHunks n (drop n l1) (drop n l2)


before pred list
   | null (tail list) = []
   | pred (head (tail list)) = [head list] ++ before pred (tail list)
   | otherwise = before pred (tail list)



