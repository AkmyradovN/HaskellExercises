import Data.Char
import Data.List
import Data.Maybe

--  1) Indices of empty lists
--  Implement a function, which given a list of lists, returns the indices of the lists that are empty. 
--  You can assume, that the input list is finite, but it can potentially contain infinite lists.
--  Indexing start from 1.
index :: Eq a => [[a]] -> Int -> [Int]
index [] n = []
index (x:xs) n 
    | x == [] = [n] ++ index xs (n+1)
    | otherwise = index xs (n+1)
 
indicesOfEmpties :: Eq a => [[a]] -> [Int]
indicesOfEmpties [] = []
indicesOfEmpties ls = index ls 1

-- 2) Modification of words
-- Apply a function of type String -> String on every word in a sentence. 
-- The sentence is assumed to be finite.
-- Hint: Use the words and unwords functions!

applyOnWords :: (String -> String) -> String -> String
applyOnWords f st = unwords(map f (words st))

-- 3) Replace all occurrences 
-- Replace all occurrences of a given element in a list with a given substitution! 
-- You can assume that the list is finite.

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll a [] b = []
replaceAll a (x:xs) b
    | a == x = b:replaceAll a xs b
    | otherwise = x:replaceAll a xs b

-- 4) Conditional transformation
-- Define a function with three parameters:
--  * a predicate (condition)
--  * a function
--  * a list
-- The function should start applying the function to the elements of the list 
-- starting from the beginning and do so as long as the predicate holds 
-- (the condition is satisfied). As soon as an element doesn't meet the condition, 
-- the transformation should stop and the rest of the list (including the element) should be left intact.

applyWhile' :: (a -> Bool) -> (a -> a) -> [a] -> Bool -> [a]
applyWhile' f1 f2 [] c = []
applyWhile' f1 f2 (x:xs) c
    | f1 x && c = (f2 x):applyWhile' f1 f2 xs c
    | otherwise = x:applyWhile' f1 f2 xs False

applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhile f1 f2 ls = applyWhile' f1 f2 ls True

-- 5) Fixed point search
-- Define a function, which determines whether a given function reaches a fixed point 
-- from a given starting point in a given number of steps. In case a fixed point is found,
--  return the number of required steps wrapped in a Just constructor. (If the initial value 
--  is already a fixed point, the required number of steps is 0.) Otherwise (if no fixed point
-- is found under the given step limit) return Nothing.
-- Fixed point: A certain value is considered to be a fixed point of a function, if the function
-- maps the value to itself, meaning that executing the function with that value as an input
-- results in that exact same value as output.
-- For example, (-1) is a fixed point of (\x -> 3*x+2), since (3 * (-1) + 2) = (-1)
-- Hint: Use an auxiliary function!


fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int
fixedPointIn f x s
    | s < 0 = Nothing
    | otherwise = aux f x s 0
    where 
        aux f x s count
            | count > s = Nothing
            | f x == x = Just count
            | otherwise = aux f (f x) s (count + 1)

-- 6) Character search
-- You are given a text that we would like to print and a list of characters that are available in your font. Implement a function that determines whether it is possible or not, and if not, what characters are missing.
-- 
-- We don't care about capitalization, so the check should be done in a case-insensitive manner. (Hint: Use toLower from Data.Char!)
-- 
-- If there are no missing letters, return Nothing, otherwise return the list of missing characters wrapped in a Just constructor. The order of the output does not matter, but it should only contain each character once! (Hint: Use nub from Data.List!)
-- 
-- You can assume that the text and the character set are both finite.

lackOfLetters :: String -> [Char] -> Maybe [Char]
lackOfLetters st ls
    | lackOfLetters' st ls == [] = Nothing
    | otherwise = Just (nub(lackOfLetters' st ls))
    where 
        lackOfLetters' [] ls = []
        lackOfLetters' (x:xs) ls
            | isMember (toLower x) ls = lackOfLetters' xs ls
            | otherwise = toLower x: lackOfLetters' xs ls
                where 
                    isMember :: Char -> [Char] -> Bool
                    isMember c [] = False
                    isMember c (x:xs)
                        | c == x = True
                        |otherwise = isMember c xs

-- 7) Function with highest return value
-- Given a list of functions and a fixed input value, determine which function results
-- in the highest output value when executed on the given input and return its position 
-- (indexed from 1) in the list, wrapped in a Just constructor! If multiple functions 
-- share the highest value, return the index of the last one. If the list of functions
--  was empty, return Nothing.
-- Hint: Use zip or an auxiliary function!

maxVal :: Ord b => [a -> b] -> a -> Int -> Int -> (Int, Int)
maxVal [] _ x y = (x,y)
maxVal (f:[]) n x y = (x,y)
maxVal (f:g:gx) n x y
    | (f n) <= (g n) = maxVal (g:gx) n (x+1) (x+1)
    | otherwise = maxVal (g:gx) n (x+1) y

maxValFun :: Ord b => [a -> b] -> a -> Maybe Int
maxValFun [] _ = Nothing
maxValFun fs x = Just $ snd $ maxVal fs x 1 1