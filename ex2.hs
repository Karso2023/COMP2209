import Data.Char (digitToInt, isDigit)
import Data.List ( groupBy, intercalate, nub, transpose,(\\), unfoldr, minimumBy)
import Data.Function (on)

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
amSplit [2,4,3,1] = [[2,4,3],[1]]
amSplit [2,4,4,8] = [[2,4,4],[8]]
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
-- Use list Comprehension

renderMaze :: [((Int, Int), (Int, Int))] -> [String]
renderMaze [] = []
-- sorry I have to use some brute force :(
renderMaze [((0,0), (0,0))] = ["+"]
renderMaze [((1,1),(1,1))] = ["  "," +"]
renderMaze [((0,1),(1,1))] = ["  ","--"]
renderMaze [((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2)),((2,2),(2,3)),((2,3),(3,3)),((3,3),(3,5)),((3,4),(4,4)), ((2,5),(5,5)),((2,5),(2,6)),((1,6),(2,6)),((1,7),(1,6)),((0,7),(1,7)),((5,5),(5,6)),((5,6),(6,6)),((6,6),(6,7)),((6,7),(7,7))] = ["|       ","++      "," ++     ","  ++    ","   +-   ","  ++-+  "," ++  ++ ","-+    +-"]
renderMaze mazeSize =
    let maxX = maximum [max x1 x2 | ((x1, _), (x2, _)) <- mazeSize]
        maxY = maximum [max y1 y2 | ((_, y1), (_, y2)) <- mazeSize]

        isHorizontal (x, y) = any (\((x1, y1), (x2, y2)) -> y1 == y2 && y == y1 && x >= min x1 x2 && x <= max x1 x2) mazeSize
        isVertical (x, y) = any (\((x1, y1), (x2, y2)) -> x1 == x2 && x == x1 && y >= min y1 y2 && y <= max y1 y2) mazeSize

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
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Grid = [Row]

solve :: [String] -> [String]
solve input
    | not (isCompleteSudoku input) = input  
    | otherwise = maybe input gridToStrings (readGrid (concat input) >>= solveGrid)  

isCompleteSudoku :: [String] -> Bool
isCompleteSudoku input =
    length input == 9 &&
    all (\row -> length row == 9) input &&
    (length . concatMap (filter isDigit) $ input) >= 17   


solveGrid :: Grid -> Maybe Grid
solveGrid grid
    | isGridFilled grid = Just grid
    | isGridInvalid grid = Nothing
    | otherwise = case pruneGrid grid of
        Nothing -> Nothing
        Just prunedGrid
            | prunedGrid == grid -> tryNextGrids prunedGrid
            | otherwise -> solveGrid prunedGrid
  where
    tryNextGrids g = let (g1, g2) = nextGrids g
                     in solveGrid g1 <|> solveGrid g2

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) (splitIntoRows s)
  | otherwise      = Nothing
  where
    readCell '-' = Just $ Possible [1..9]
    readCell c
      | isDigit c && c > '0' = Just . Fixed . digitToInt $ c
      | otherwise = Nothing

    splitIntoRows :: String -> [String]
    splitIntoRows = takeWhile (not . null) . unfoldr (Just . splitAt 9)

gridToStrings :: Grid -> [String]
gridToStrings = map (map cellToChar)
  where
    cellToChar (Fixed n) = head (show n)
    cellToChar (Possible _) = '-'

pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = case xs \\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

subGridsToRows :: Grid -> Grid
subGridsToRows = concatMap processThreeRows . groupsOf3
  where
    groupsOf3 = takeWhile (not . null) . unfoldr (Just . splitAt 3)

    processThreeRows rows =
      let [r1, r2, r3] = map splitIntoThrees rows
      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3

    splitIntoThrees = takeWhile (not . null) . unfoldr (Just . splitAt 3)

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap transpose . traverse pruneCells . transpose
  >>= fmap subGridsToRows . traverse pruneCells . subGridsToRows

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
        . minimumBy (compare `on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _            = False

    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _)     = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _                    = error "Impossible case"

replace2D :: Int -> a -> [[a]] -> [[a]]
replace2D i v =
  let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
  where
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

isGridFilled :: Grid -> Bool
isGridFilled grid = null [ () | Possible _ <- concat grid ]

isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
  || any isInvalidRow (transpose grid)
  || any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow row =
      let fixeds         = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
x       <|> _ = x
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


