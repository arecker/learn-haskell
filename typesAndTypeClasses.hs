removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase str = [ x | x <- str, x `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumferance :: Float -> Float
circumferance r = r * 2 * pi

circumferance' :: Double -> Double
circumferance' r = r * 2 * pi
