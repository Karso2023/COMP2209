module Q2 where 

-- Your Imports Here
import Data.List ((\\), nub, sort)
import qualified Data.Set as Set
import Data.Set (Set)

-- DO NOT MODIFY THESE DATA TYPES
data EdgeDir = L| R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)
-----------------------

-- Original calcInteractions implementation (unchanged as it works)
calcInteractions :: Int -> [Atom] -> [(EdgePoint, EdgePoint)]
calcInteractions size atoms = 
    [(entry, traceRayPath entry (getInitialPosition entry)) | entry <- allEntryPoints]
  where
    atomSet = Set.fromList atoms
    allEntryPoints = [EP face pos dir | face <- [East, West, South],
                                      pos <- [1..size],
                                      dir <- [L, R]]

    getInitialPosition (EP face idx dir) = case face of
        South -> (size, (2 * idx) - 1)
        West  -> (idx, 1)
        East  -> (idx, 2 * idx)

    calculateNextStep (EP face _ dir) (x, y) = 
        case (face, dir) of
            (West, L)  -> if even y then (x, y+1) else (x+1, y+1)
            (West, R)  -> (x, y+1)
            (East, L)  -> (x, y-1)
            (East, R)  -> if even y then (x, y-1) else (x+1, y+1)
            (South, L) -> if even y then (x-1, y-1) else (x, y+1)
            (South, R) -> if even y then (x-1, y-1) else (x, y-1)

    isOutsideGrid (x, y) = x < 1 || x > size || y < 1 || y > (2 * x)

    reflectAtAtom (EP f i d) (x, y)
        | even y    = EP (reflectEven f d) i d
        | otherwise = EP (reflectOdd f d) i d
      where
        reflectEven West  L = South
        reflectEven West  R = East
        reflectEven East  L = West
        reflectEven East  R = South
        reflectEven South L = East
        reflectEven South R = West

        reflectOdd West  L = East
        reflectOdd West  R = South
        reflectOdd East  L = South
        reflectOdd East  R = West
        reflectOdd South L = West
        reflectOdd South R = East

    calculateExitPoint (EP f idx d) (x, y) =
        let exitFace = getExitFace f d
            newDir = reverseDir d
        in if exitFace == South 
           then EP South ((y + 1) `div` 2) newDir
           else EP exitFace x newDir
      where
        getExitFace West  L = South
        getExitFace West  R = East
        getExitFace East  L = West
        getExitFace East  R = South
        getExitFace South L = East
        getExitFace South R = West

        reverseDir L = R
        reverseDir R = L

    traceRayPath ep pos@(x, y)
        | isOutsideGrid nextPos = calculateExitPoint ep pos
        | pos `Set.member` atomSet =
            let ep' = reflectAtAtom ep pos
                nextPos' = calculateNextStep ep' pos
            in traceRayPath ep' nextPos'
        | nextPos `Set.member` atomSet =
            let ep' = reflectAtAtom ep nextPos
            in traceRayPath ep' pos
        | otherwise = traceRayPath ep nextPos
        where
         nextPos = calculateNextStep ep pos

-- New functions for finding solution
combinations4Fast :: [a] -> [[a]]
combinations4Fast xs = do
    (i1, x1) <- zip [0..] xs
    (i2, x2) <- zip [(i1+1)..] (drop (i1+1) xs)
    (i3, x3) <- zip [(i2+1)..] (drop (i2+1) xs)
    (i4, x4) <- zip [(i3+1)..] (drop (i3+1) xs)
    return [x1, x2, x3, x4]

-- Main solver function
solveTBB :: Int -> [(EdgePoint, EdgePoint)] -> [Atom]
solveTBB size interactions = 
    let target = sort interactions
        maxX = size * 3  -- Allow for larger X values
        validPositions = [(x,y) | x <- [1..maxX], 
                                y <- [1..x*3],  -- Allow for larger Y values
                                x <= maxX,       -- Ensure within bounds
                                y >= 1]
        allConfigs = combinations4Fast validPositions
    in head [atoms | atoms <- take 10000 allConfigs,  -- Limit search space
                    sort (calcInteractions size atoms) == target]


