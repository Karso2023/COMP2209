-- Continuing with the stack machine SM from Exercise A7, 
-- we say that a sequence of instructions containing only Dup and Mul instructions is called a power sequence for k  where k > 0 
-- if, given an initial stack containing any single initial value Just a, evaluating the instruction sequence leaves the single value Just (a^k) on the stack.
-- We say that a power sequence for k has Dup length n if it contains exactly n Dup instructions.

-- Write a function  possiblePowers :: (Int,Int) -> (Int,Int) -> [(Int,Int)] that, 
-- given input values (kmin,kmax) and (nmin,nmax) say, returns all paris (k,n) where kmin <= k <= kmax and nmin <= n <= nmax such that there exists a power sequence for k of Dup length n. 

-- Your own imports here
import Control.Applicative ((<|>))

-- DO NOT MODIFY
data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show)
type Stack = [Maybe Integer]
type SMProg = [Instruction]
-- DO NOT MODIFY

-- Helper function to evaluate a program
evaluate :: Stack -> SMProg -> Stack
evaluate [] _ = []
evaluate (x:xs) [] = x:xs
evaluate (Just x : Just y : xs) (Mul:rest) = evaluate (Just (x * y) : xs) rest
evaluate (x:xs) (Dup:rest) = evaluate (x : x : xs) rest
evaluate _ _ = []  -- Invalid operation or empty stack

-- Main function to find possible powers
possiblePowers :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
possiblePowers (kmin, kmax) (nmin, nmax) =
    [(k, n) | k <- [kmin..kmax], n <- [nmin..nmax], isPowerSequence k n]

-- Check if there exists a power sequence for k with Dup length n
isPowerSequence :: Int -> Int -> Bool
isPowerSequence k n = dfs [] initialState isGoal expand /= Nothing
  where
    initialState = ([Just 2], 0)
    isGoal (stack, dupCount) =
      case stack of
        [Just result] -> result == 2^k && dupCount == n
        _ -> False

    expand (stack, dupCount)
      | dupCount < n = [(evaluate stack [Dup], dupCount + 1), (evaluate stack [Mul], dupCount)]
      | otherwise    = [(evaluate stack [Mul], dupCount)]

-- Depth-first search implementation
dfs :: [(Stack, Int)] -> (Stack, Int) -> ((Stack, Int) -> Bool) -> ((Stack, Int) -> [(Stack, Int)]) -> Maybe (Stack, Int)
dfs visited current isGoal expand
  | isGoal current = Just current
  | current `elem` visited = Nothing
  | otherwise = foldr (\next acc -> acc <|> dfs (current : visited) next isGoal expand) Nothing (expand current)




