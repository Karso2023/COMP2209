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

-- Ex5

-- Ex6

-- Ex7

-- Ex8

-- Ex9