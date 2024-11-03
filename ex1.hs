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

encrypt :: Int -> String -> (String, String -> String)
encrypt n s = (enc n s, decrypt)
 where decrypt = enc (-n)

-- Ex6
meetsOffer :: String -> Int -> Bool
meetsOffer grades offer = sumGrades grades >= offer
   where sumGrades [] = 0
         sumGrades ('A':'*':grades) = 56 + sumGrades grades
         sumGrades (g:grades) | 'A' <= g && g <= 'E' = ((ord 'E' - ord g)+2)*8  + sumGrades grades
                              | otherwise = sumGrades grades
                              

-- Ex7
luhnDouble :: Int -> Int
luhnDouble n | m > 9 = m - 9
             | otherwise = m 
  where m = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 = 
     (luhnDouble n1 + n2 + luhnDouble n3 + n4) `mod` 10 == 0