-- Tree

data BinaryTree a = Leaf a | Node (BinaryTree a) (BinaryTree a) deriving (Show)

data Direction a = L (BinaryTree a) | R (BinaryTree a) deriving (Show)
type Trail a = [Direction a]

parentInDirection :: Direction a -> BinaryTree a
parentInDirection (L p) = p
parentInDirection (R p) = p

goLeft :: (BinaryTree a, Trail a) -> (BinaryTree a, Trail a)
goLeft (Leaf x, ts) = (Leaf x, ts)
goLeft (p@(Node l _) , ts) = (l, L p:ts)

goRight :: (BinaryTree a, Trail a) -> (BinaryTree a, Trail a)
goRight (Leaf x, ts) = (Leaf x, ts)
goRight (p@(Node _ r), ts) = (r, R p:ts)

goUp :: (BinaryTree a, Trail a) -> (BinaryTree a, Trail a)
goUp (tree, ts) = (parent, restOfList)
    where
        lastMove = head ts
        restOfList = tail ts
        parent = parentInDirection lastMove

-- Graph
data GGraph a = GGNode a [GGraph a]

mkGGraph :: [(a, [Int])] -> GGraph a
mkGGraph table = head table'
    where table' =
           map (\(x,ns) -> GGNode x (map (table' !!) ns)) table

-- Alpha conversion and Beta reduction 



{-
    Call-By-Name
        - Call by name is a specific evaluation strategy that passes arguments without evaluating them first
        - Uses outermost reduction
            - outermost reduction = reduce leftmost, outermost redex first
        - Has no caching of results
        - pure lazy substitution (Haskell also uses graph reduction to perfrom lazy evaluation)

    Example:
        (\x -> x + x) (2 + 3)
        (2 + 3) + (2 + 3)    -- substitute unevaluated (2+3)
        5 + (2 + 3)          -- evaluate left (2+3)
        5 + 5                -- evaluate right (2+3)
        10                   -- final result

    For an infinite list
        take 1 [1,2,3,4...], call by name doesn't care about the infinite list. It wont try to 
        process it. It will just perform the function "take 1" and take 1 element off that list.
-}

{-
    Call-By-Value
            - used by languages like C++, Java, Python etc.
            - Uses innermost reduction (also called applicative order reduction)
                - evaluate the innermost expression first 
                - then work our way outwards
                - arguments are evaluated before being passed to functions
                - no caching 
                - pure eager evaluation

    Example:
        (\x -> x + x) (2 + 3)
        (\x -> x + x) 5      -- evaluate (2+3) first
        5 + 5                -- substitute 5
        10                   -- final result    

    For an infinite list
        take 1 [1,2,3,4...],  call-by-value will try and process that infinite list, and it’ll be there forever because 
        there’s no end to it. Therefore, this function call will not end
-}

{-
    - A variable x is bound in the term λx → e and the scope of the binding is the 
    expression e.
    - A variable y is free in the term λx → y because it was initialised outside of the scope 
    of this function abstraction.
    - A lambda expression with no free variables is called closed
-}


