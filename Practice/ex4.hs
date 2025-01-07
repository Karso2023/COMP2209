-- Ex1
sumEvenOdd :: Integer
sumEvenOdd = sum [ x^2 | x <- [0,2..100] ] + sum [x^3 | x <- [1,3..99]]

-- Ex2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Ex3
replicate' :: Int -> a -> [a]
replicate' i s= [s | _ <- [1..i]]

-- Ex4
pyths :: Int -> [(Int, Int, Int)]
pyths p = [(x,y,z) | x <- [1..p], y <- [1..p], z <- [1..p], x^2 + y^2 == z^2]

-- Ex5
data Status = Pass | Fail deriving (Eq, Show, Read)

aveFailMark :: [(String,Status,Int)] -> Float


-- Ex6

-- Ex7

-- Ex8

-- Ex9