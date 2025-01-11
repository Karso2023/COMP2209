{-|
  Module      : COMP2209 Q1
  Copyright   : (c) 2025 University of Southampton
  Author      : Karso Cheung 
  Description :
  Write a Haskell function that, given the size of the grid and the location of atoms within a grid, then the function will calculate all possible ray firings
  
  My code's logic stpes: 
    1, Set the initial ray positions first to simulate all possible ray entries 
    2, Keep checking if there's atom in position (x, y), if not then then return the exit points using the default reflections
    3, If there's an atom, check if (x, y) is upward triangle (y = odd) or downward triangle (y = even) since they have different reflections 
    4, Change the positions based on the triangle sides
    5, Update the current positions and recurse to step 2
  
-}
-- module Q1 where (for Tests.hs) 
-- Your imports here
import qualified Data.Set as Set
import Data.Set (Set)
-- DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST
data EdgeDir = L | R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)
--  DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST

-- Main function to calculate interactions
calcInteractions :: Int -> [Atom] -> [(EdgePoint, EdgePoint)]
calcInteractions size atoms = 
    [(entry, traceRayPath entry (getInitialPosition entry)) | entry <- allEntryPoints]
  where
    atomSet = Set.fromList atoms
    allEntryPoints = 
        [EP face pos dir | face <- [East, West, South],
                          pos <- [1..size],
                          dir <- [L, R]]

    getInitialPosition :: EdgePoint -> (Int, Int) 
    getInitialPosition (EP face idx dir) = case face of
        South -> (size, (2 * idx) - 1)
        West  -> (idx, 1)
        East  -> (idx, 2 * idx)

    {-
        Calculate how the ray's position (x,y) should be in after reflection from different faces 
        - even = downward triangle
        - odd = upward triangle
    -}
    calculateNextStep :: EdgePoint -> (Int, Int) -> (Int, Int)
    calculateNextStep (EP face _ dir) (x, y) = 
        case (face, dir) of
            (East, L)  -> (x, y-1)
            (East, R)  -> if even y then (x, y-1) else (x+1, y+1)
            (West, L)  -> if even y then (x, y+1) else (x+1, y+1)
            (West, R)  -> (x, y+1)
            (South, L) -> if even y then (x-1, y-1) else (x, y+1)
            (South, R) -> if even y then (x-1, y-1) else (x, y-1)

    -- Check if the ray (x,y) is outside the size n triangular black box grid
    isOutsideGrid :: (Int, Int) -> Bool
    isOutsideGrid (x, y) = x < 1 || x > size || y < 1 || y > (2 * x)

    -- A function to give the correct reflections Face when facing upward / downward triangle following the rules (images) provided in the spec
    reflectAtAtom :: EdgePoint -> (Int, Int) -> EdgePoint
    reflectAtAtom (EP f i d) (x, y)
        | even y    = EP (reflectEven f d) i d
        | otherwise = EP (reflectOdd f d) i d
      where
        -- Reflection rules for downward triangle
        reflectEven :: Face -> EdgeDir -> Face
        reflectEven West  L = South
        reflectEven West  R = East
        reflectEven East  L = West
        reflectEven East  R = South
        reflectEven South L = East
        reflectEven South R = West

        -- Reflection rules for upward triangle
        reflectOdd :: Face -> EdgeDir -> Face
        reflectOdd West  L = East
        reflectOdd West  R = South
        reflectOdd East  L = South
        reflectOdd East  R = West
        reflectOdd South L = West
        reflectOdd South R = East

    -- Important on how the ray should reflect when facing the final position 
    -- (for example if (1,1) is the exit point, then check the face and give the correct reflection based on it)
    calculateExitPoint :: EdgePoint -> (Int, Int) -> EdgePoint
    calculateExitPoint (EP f idx d) (x, y) =
        let exitFace = getExitFace f d
            newDir = reverseDir d -- basically (L / R) -> (R / L)
        in if exitFace == South 
           then EP South ((y + 1) `div` 2) newDir
           else EP exitFace x newDir
      where
        -- A straightforward reflection based on the rules provided from the spec
        getExitFace :: Face -> EdgeDir -> Face
        getExitFace West  L = South
        getExitFace West  R = East
        getExitFace East  L = West
        getExitFace East  R = South
        getExitFace South L = East
        getExitFace South R = West

        -- (Left / Right) -> (Right / Left)
        reverseDir :: EdgeDir -> EdgeDir
        reverseDir L = R
        reverseDir R = L

    -- Important function to trace the ray position recusively and handle ray reflections off atoms
    traceRayPath :: EdgePoint -> (Int, Int) -> EdgePoint
    traceRayPath ep pos@(x, y)
        | isOutsideGrid nextPos = calculateExitPoint ep pos -- if the next position will be outside the box, then calculate the exit position
        
        -- Case 1: if there's an atom at current position:
        -- 1. Calculate new direction after reflection (ep')
        -- 2. Calculate next position based on new direction
        -- 3. Continue tracing from this position with new direction
        | pos `Set.member` atomSet =
            let ep' = reflectAtAtom ep pos
                nextPos' = calculateNextStep ep' pos
            in traceRayPath ep' nextPos'
        
        -- Case 2: if there's an atom at next position:
        -- 1. Calculate reflection from the next position
        -- 2. Continue tracing from current position with new direction
        | nextPos `Set.member` atomSet =
            let ep' = reflectAtAtom ep nextPos
            in traceRayPath ep' pos
            
        -- Case 3: ray doesn't meet any atoms
        | otherwise = traceRayPath ep nextPos
        where
         nextPos = calculateNextStep ep pos
