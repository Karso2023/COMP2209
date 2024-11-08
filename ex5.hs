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