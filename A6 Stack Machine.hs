import Control.Applicative
-- YOUR IMPORTS HERE
import Data.List ( maximumBy, subsequences )
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
-- DO NOT MODIFY 
data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show)
type Stack = [Maybe Integer]
type SMProg = [Instruction]
-- DO NOT MODIFY

evaluate :: Stack -> SMProg -> Stack
evaluate [] _ = error "Empty Stack"
evaluate (n:m:ns) (Add : ins)        = evaluate ( (pure (+) <*> n <*> m) : ns) ins
evaluate (n:m:ns) (Sub : ins)        = evaluate ( (pure (-) <*> n <*> m) : ns) ins
evaluate (n:m:ns) (Mul : ins)        = evaluate ( (pure (*) <*> n <*> m) : ns) ins
evaluate (_:(Just 0):ns) (Div : ins) = evaluate (Nothing:ns) ins
evaluate (n:m:ns) (Div : ins)        = evaluate ( (pure div <*> n <*> m) : ns) ins
evaluate (n:ns) (Dup : ins)          = evaluate (n : n : ns) ins
evaluate (n:ns) (Pop : ins)          = evaluate ns ins
evaluate ns [] = ns
evaluate _ _ = error "Invalid Operation"


-- Use evaluate to check the largest sequence
-- List all combinations using DFS and find the largest single value
-- Return back to find findMaxReductionSequence

findMaxReductionSequence :: Stack -> [SMProg]
findMaxReductionSequence [] = []
findMaxReductionSequence stack = 
    let validInstructions = [Add, Sub, Mul, Div, Pop]
        (maxValue, maxProgs) = dfs stack [] Nothing []
    in maxProgs

dfs :: Stack -> SMProg -> Maybe Integer -> [SMProg] -> (Maybe Integer, [SMProg])
dfs [Just x] prog (Just maxVal) maxProgs
    | x > maxVal = (Just x, [reverse prog])
    | x == maxVal = (Just maxVal, reverse prog : maxProgs)
    | otherwise = (Just maxVal, maxProgs)
dfs [Just x] prog Nothing _ = (Just x, [reverse prog])
dfs stack prog maxVal maxProgs =
    foldl updateMax (maxVal, maxProgs) $ 
        catMaybes [tryInstruction ins stack prog maxVal maxProgs | ins <- [Add, Sub, Mul, Div, Pop]]
  where
    updateMax (currMax, currProgs) (newMax, newProgs)
        | newMax > currMax = (newMax, newProgs)
        | newMax == currMax = (currMax, currProgs ++ newProgs)
        | otherwise = (currMax, currProgs)

tryInstruction :: Instruction -> Stack -> SMProg -> Maybe Integer -> [SMProg] -> Maybe (Maybe Integer, [SMProg])
tryInstruction ins stack prog maxVal maxProgs =
    case evaluate stack [ins] of
        [Just x] -> Just $ dfs [Just x] (ins:prog) maxVal maxProgs
        newStack@(_:_:_) -> Just $ dfs newStack (ins:prog) maxVal maxProgs
        _ -> Nothing




