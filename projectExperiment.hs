import Data.Char
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = m : toDigitsRev d
  where
    (d, m) = n `divMod` 10
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' (x:y:zs) = x : 2 * y : doubleEveryOther' zs
doubleEveryOther' (x:[])   = [x]
doubleEveryOther' []       = []
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigits'
  where
    sumDigits' n
      | n < 10    = n
      | otherwise = sumDigits $ toDigitsRev n
validate :: Integer -> Bool
validate n = 0 == (sumDigits $ doubleEveryOther' $ toDigitsRev n) `mod` 10