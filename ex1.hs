import Data.Char (chr, ord)

-- Ex1
last' :: [Int] -> Int
last' xs = xs !! (length xs -1)

last'' :: [Int] -> Int
last'' xs = head (drop n xs)
  where n = length xs - 1

-- Ex2 
fourth :: [a] -> a
fourth = (!! 3)

fourth' :: [a] -> a
fourth' xs = head (tail (tail (tail xs)))

fourth'' :: [a] -> a
fourth'' (_:_:_:n:_) = n

-- Ex3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- Ex4 
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt n xs
  where n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs = (take n xs , drop n xs)
 where n = length xs `div` 2

-- Ex5
enc :: Int -> String -> String
enc x = map (\ n -> chr (ord n + x))

enc' :: Int -> String -> String
enc' x [] = []
enc' x (n:ns) = chr(ord n + x) : enc' x ns  

--encrypt :: Int -> String -> (String, String -> String)



