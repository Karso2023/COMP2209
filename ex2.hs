import Data.Char
-- Exercise 1

-- Exercise 2 
fourth :: [a] -> a
fourth xs | length xs < 4 = error "Length of list < 4" 
          | otherwise = xs !! 3

-- Exercise 3
safetail :: [a] -> [a]
safetail xs | null xs = []  
            | otherwise = tail xs

 where null xs = length xs == 0 

 -- Exercise 4
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length xs `div` 2) xs


-- Exercise 5
-- enc :: Int -> String -> String 


-- Assessed Exercises A1
-- group anti-monotone 
-- anti-monotone if it has no contiguous monotone sublist with 3 distinct values
amSplit :: [Int] -> [[Int]]
amSplit [] = []
amSplit [a] = [[a]]
amSplit xs | length xs < 3 = [xs]
           | 


-- Assessed Exercises A2

-- Assessed Exercises A3