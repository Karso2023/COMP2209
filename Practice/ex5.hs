flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x:flatten (xs:vs)

reverseFlatten :: [a] -> [[a]]
reverseFlatten [] = []
reverseFlatten xs = [xs]

-- -- --
isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

-- -- --
absValue :: Int -> Int 
absValue x
     | x > 0 = x 
     | otherwise = -x 

-- -- --
-- Double linked list 
data DLList a = Empty | Node a (DLList a) (DLList a)
mkDLList :: [a] -> DLList a 
mkDLList [] = Empty 
mkDLList [x] = Node x Empty Empty
mkDLList [x1, x2] = let node1 = Node x1 Empty node2 
                        node2 = Node x2 node1 Empty
                    in node1

mkDLList [x1, x2, x3] = let node1 = Node x1 Empty node2
                            node2 = Node x2 node1 node3
                            node3 = Node x3 node2 Empty
                        in node1