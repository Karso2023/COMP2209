module Q1 where 

import qualified Data.Set as Set
import Data.Set (Set)

-- DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST
data EdgeDir = L | R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)
--  DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST

data RayDir = WestRight | WestLeft | SouthLeft | EastLeft | EastRight | SouthRight
    deriving (Eq, Show, Ord)

-- Main function to calculate interactions
calcInteractions :: Int -> [Atom] -> [(EdgePoint, EdgePoint)]
calcInteractions size atoms =
    [(entry, traceRayPath entry (getInitialPosition entry)) | entry <- allEntryPoints]
  where
    atomSet = Set.fromList atoms
    allEntryPoints = [EP face pos dir | face <- [East, West, South],
                                       pos <- [1..size],
                                       dir <- [L, R]]

    -- Get initial position for a ray
    getInitialPosition :: EdgePoint -> (Int, Int)
    getInitialPosition (EP South idx _) = (size, (2 * idx) - 1)
    getInitialPosition (EP West  idx _) = (idx, 1)
    getInitialPosition (EP East idx _) = (idx, 2 * idx)

    -- Calculate next position in grid
    calculateNextStep :: EdgePoint -> (Int, Int) -> (Int, Int)
    calculateNextStep (EP face _ dir) (x, y) = case (face, dir) of
        (West, L)  -> if even y then (x, y+1) else (x+1, y+1)
        (West, R)  -> (x, y+1)
        (East, L)  -> (x, y-1)
        (East, R)  -> if even y then (x, y-1) else (x+1, y+1)
        (South, L) -> if even y then (x-1, y-1) else (x, y+1)
        (South, R) -> if even y then (x-1, y-1) else (x, y-1)

    -- Check if position is outside grid
    isOutsideGrid :: (Int, Int) -> Bool
    isOutsideGrid (x, y) = x < 1 || x > size ||
                          y < 1 || y > (2 * x)

    -- Calculate reflection at atom
    reflectAtAtom :: EdgePoint -> (Int, Int) -> EdgePoint
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

    -- Calculate exit point
    calculateExitPoint :: EdgePoint -> (Int, Int) -> EdgePoint
    calculateExitPoint (EP f idx d) (x, y)
        | getExitFace f d == South = EP South ((y + 1) `div` 2) (reverseDir d)
        | otherwise = EP (getExitFace f d) x (reverseDir d)
      where
        getExitFace West  L = South
        getExitFace West  R = East
        getExitFace East  L = West
        getExitFace East  R = South
        getExitFace South L = East
        getExitFace South R = West

        reverseDir L = R
        reverseDir R = L

    -- Trace ray path through grid
    traceRayPath :: EdgePoint -> (Int, Int) -> EdgePoint
    traceRayPath ep (x, y)
        | isOutsideGrid (calculateNextStep ep (x, y)) = calculateExitPoint ep (x, y)
        | (x, y) `Set.member` atomSet =
            let ep' = reflectAtAtom ep (x, y)
                next = calculateNextStep ep' (x, y)
            in traceRayPath ep' next
        | let next = calculateNextStep ep (x, y)
        , next `Set.member` atomSet =
            let ep' = reflectAtAtom ep next
            in traceRayPath ep' (x, y)
        | otherwise = traceRayPath ep (calculateNextStep ep (x, y))