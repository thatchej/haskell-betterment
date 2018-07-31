#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.Char
import Data.List

main :: IO ()
main = putStrLn (show (largestNumber [1, 12, 15, 12, 1, 4]))

-- returns a string with only duplicate chars from original string
dupChars :: String -> String
dupChars [] = []
dupChars (x:xs) 
    | x `elem` xs = x : dupChars xs
    | otherwise   = dupChars xs

-- returns true if string is palindrome, false otherwise
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome xs
    | cleanString xs == jarReverse (cleanString xs) = True
    | otherwise                                     = False

-- returns the missing number in an array of [1..100]
missingNumber :: [Integer] -> Int
missingNumber x = case elemIndex False (map checkTupleEq (zipWithPadding x [1..100])) of
    Just n  -> n + 1
    Nothing -> 0

missingNumber' :: [Integer] -> Integer
missingNumber' x = sum [1..100] - sum x

-- returns largest number in array
largestNumber :: [Integer] -> Integer
largestNumber [] = 0
largestNumber (x:xs) 
    | x > largestNumber xs = x
    | otherwise            = largestNumber xs

largestNumber' :: [Integer] -> Integer
largestNumber' = foldr1 (\x y -> if x >= y then x else y) 

-- HELPER FUNCTIONS
--
checkTupleEq :: (Integer, Integer) -> Bool
checkTupleEq x = if fst x == snd x then True else False

-- normal zip, but will zip w/ empty list if lengths not equal
zipWithPadding :: [Integer] -> [Integer] -> [(Integer,Integer)]
zipWithPadding (x:xs) (y:ys) = (x,y) : zipWithPadding xs ys
zipWithPadding [] ys         = zip (repeat 0) ys
zipWithPadding xs []         = zip xs (repeat 0)

-- converts Bool to String representation
boolToString :: Bool -> String
boolToString x = if x then "True" else "False"

-- reverses a string
jarReverse :: String -> String
jarReverse [] = []
jarReverse x  = last x : jarReverse (init x)

-- returns a lowercase version of String arg w/ no spaces
cleanString :: String -> String
cleanString x = filter (/=' ') (map toLower x)