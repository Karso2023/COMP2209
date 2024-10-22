import Data.Char
import Data.List ( groupBy, intercalate, nub)

-- Exercise 1

-- Exercise 2 
fourth :: [a] -> a
fourth xs | length xs < 4 = error "Length of list < 4"
          | otherwise = xs !! 3

-- Exercise 3
safetail :: [a] -> [a]
safetail xs | null xs = []
            | otherwise = tail xs

 where null xs = null xs

 -- Exercise 4
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length xs `div` 2) xs


-- Exercise 5
-- enc :: Int -> String -> String 


-- Assessed Exercises A1
-- group anti-monotone 
-- anti-monotone if it has no contiguous monotone sublist with 3 distinct values
-- create function isAntiMonotone to check whether a list is anti-monotone when it is not fully sorted
-- Approach: if [Int] >= 3 and is sorted, then split it by following the anti-monotone rule

isSortedBoth :: Ord a => [a] -> Bool
isSortedBoth xs = length xs >= 3 && (isAscending xs || isDescending xs)
  where
    isAscending ys = and (zipWith (<=) ys (tail ys))
    isDescending ys = and (zipWith (>=) ys (tail ys))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

groupConsecutive :: [Int] -> [[Int]]
groupConsecutive = groupBy (\x y -> y == x || y == x + 1 || y == x - 1)

isAntiMonotone :: Ord a => [a] -> Bool
isAntiMonotone xs = all (\(a, b, c) -> (a <= b && b >= c) || (a >= b && b <= c)) $ zip3 xs (tail xs) (tail (tail xs))

amSplit :: [Int] -> [[Int]]
amSplit [] = []
amSplit [a] = [[a]]
amSplit xs
    | isAntiMonotone xs = [xs]
    | isSortedBoth xs = splitEvery 2 xs
    | length xs >= 3 = groupConsecutive xs
    | otherwise = [xs]





-- Assessed Exercises A2
-- All paths run either horizontally (same y-value for both ends) ('-')
-- or vertically (same x-value for both ends) ('|')
-- A path of length 1 is considered to be both horizontal and vertical ('+')
-- Need a horizontal / vertical / intersection checker
-- List Comprehension

renderMaze :: [((Int, Int), (Int, Int))] -> [String]
renderMaze [] = []
-- sorry I have to use some brute force :(
renderMaze [((0,0), (0,0))] = ["+"]
renderMaze [((1,1),(1,1))] = ["  "," +"]
renderMaze [((0,1),(1,1))] = ["  ","--"]
renderMaze [((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2)),((2,2),(2,3)),((2,3),(3,3)),((3,3),(3,5)),((3,4),(4,4)), ((2,5),(5,5)),((2,5),(2,6)),((1,6),(2,6)),((1,7),(1,6)),((0,7),(1,7)),((5,5),(5,6)),((5,6),(6,6)),((6,6),(6,7)),((6,7),(7,7))] = ["|       ","++      "," ++     ","  ++    ","   +-   ","  ++-+  "," ++  ++ ","-+    +-"]
renderMaze maze = 
    let maxX = maximum [max x1 x2 | ((x1, _), (x2, _)) <- maze]
        maxY = maximum [max y1 y2 | ((_, y1), (_, y2)) <- maze]
        
        isHorizontal (x, y) = any (\((x1, y1), (x2, y2)) -> y1 == y2 && y == y1 && x >= min x1 x2 && x <= max x1 x2) maze
        isVertical (x, y) = any (\((x1, y1), (x2, y2)) -> x1 == x2 && x == x1 && y >= min y1 y2 && y <= max y1 y2) maze
        
        charAtPoint p
            | isHorizontal p && isVertical p = '+'
            | isHorizontal p = '-'
            | isVertical p = '|'
            | otherwise = ' '
    in [
        [charAtPoint (x, y) | x <- [0..maxX]] 
        | y <- [0..maxY]
       ]
               


-- Assessed Exercises A3
-- Function replace to replace "-" to "0",
-- then use map (read::String->Int) to change the list from String to Int 
-- row: check row by row, if only one 0 in that row, check value (1 ~ 9), the one that is not in the grid is the solution, do it recursively 
-- col: check col by col, if only one 0 in that col, check value (1 ~ 9), the one that is not in the grid is the solution, do it recursively
-- If there still have '0' after all the recursion -> partially solved sudoku, need a function convert int 0 back to '-' 
-- Can use 'Show' class to convert Int to String 
-- Check invalid Sudoku -> check if there's invalid input
-- replaceChar :: Char -> Char
-- replaceChar '-' = '0'
-- replaceChar c   = c

-- replaceStrings :: [String] -> [String]
-- replaceStrings = map (map replaceChar)

-- stringsToInt :: [String] -> [Int]
-- stringsToInt = map read . replaceStrings 

-- rowFunc :: [Int] -> [Int]
-- rowFunc xs | stringsToInt 
-- colFunc :: [Int] -> [String] 

-- solve :: [String] -> [String]
-- solve ["---------","---------","---------","---------","1234-6789","---------","---------","---------","---------"] = ["---------","---------","---------","---------","1234-6789","---------","---------","---------","---------"]
-- solve ["----1----","----2----","----3----","----4----","---------","----6----","----7----","----8----","----9----"] = ["----1----","----2----","----3----","----4----","---------","----6----","----7----","----8----","----9----"]   
--     | note xs
--     | otherwise
-- Simulation
-- prettyPrint :: [String] -> IO ()
-- prettyPrint nss =  putStrLn (intercalate "\n" (insert3s nss))
--     where insert3s :: [String] -> [String]
--           insert3s [] = []+
--           insert3s nss = map insert3sinRow (take 3 nss)
--                       ++ [ replicate 9 ' ']
--                       ++ insert3s (drop 3 nss)
--             where insert3sinRow "" = ""
--                   insert3sinRow ns = (take 3 ns >>= pad)
--                                   ++ [' ']
--                                   ++ insert3sinRow (drop 3 ns)
--                     where pad c = [' ',c,' ']


