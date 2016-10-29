toInteger' :: String -> Integer
toInteger' x = read x :: Integer


toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
     | x < 10 = [x] 
     | otherwise = x `mod` 10 : toDigitsRev(x `div` 10)


doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft ([a]) = [a]
doubleEveryOtherFromLeft (d1:d2:ds) = d1 : (2 * d2) : doubleEveryOtherFromLeft ds             


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits ([a]) = a
sumDigits (d1:ds) | d1 < 10 = d1 + sumDigits ds 
                | otherwise = (d1 `mod` 10) + (d1 `div` 10) + sumDigits ds

validate :: String -> Bool
validate pin | length pin == 16 = (((sumDigits.doubleEveryOtherFromLeft.toDigitsRev.toInteger')pin) `mod` 10) == 0
             | otherwise =  error "Kindly check your pin...It should be exactly equal to 16 digits"