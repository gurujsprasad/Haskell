--testing n = n `div` 2
--testing n = [div | div <- takeWhile ( <= n `div` 2) [1..100] ]
--prime x = takeWhile (<=x) primes

oddNumbers = [3,5 .. ]

divisors n = [div | div <- takeWhile ( <= n `div` 2) oddNumbers, n `mod` div == 0]

primes = 2:[ p | p <- (takeWhile (<=6000) oddNumbers), divisors p == []]
isPrime y = y `elem` primes

oddNonPrimes = [ op | op <- (takeWhile (<=6000) oddNumbers), not (isPrime op)]

squareNumbers = [x*x | x <- [1..]]
isASquare s = s `elem` (takeWhile ( <=s) squareNumbers)

iSGoldBachs g = [p | p <- (takeWhile (<=g) primes), isASquare (div(g - p) 2)] == []
final = [op | op <- oddNonPrimes , iSGoldBachs op ]