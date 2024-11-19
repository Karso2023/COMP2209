import Control.Applicative
-- YOUR IMPORTS HERE

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

findMaxReductionSequence :: Stack -> [SMProg]
findMaxReductionSequence = undefined