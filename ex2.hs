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
-- Approach: if [Int] >= 3 and is sorted, then split it by following anti-monotone rule
isSortedBoth :: Ord a => [a] -> Bool
isSortedBoth xs = length xs >= 3 && (isAscending xs || isDescending xs)
  where
    isAscending ys = and (zipWith (<=) ys (tail ys))
    isDescending ys = and (zipWith (>=) ys (tail ys))

amSplit :: [Int] -> [[Int]]
amSplit [] = []
amSplit [a] = [[a]]
amSplit xs
     | isSortedBoth xs = [take n xs, drop n xs]
     | otherwise = [xs]
  where n = length xs `div` 2




-- Assessed Exercises A2

-- Assessed Exercises A3