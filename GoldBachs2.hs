oddsFrom3 = [3, 5 .. ]

listA = [3, 5 ..]
listB = [x | x <- [3, 5 ..]]
listC = [x | x <- [3, 5 .. 5999]]
listD = [x | x <- [3, 5 .. 6000]]
listE = [x | x <- [3, 5 ..], (x < 6000)]
listF = [x | x <- takeWhile (< 6000) [3, 5 ..] ]
{- 
Question: What are the differences among these lists?
a) listA = [3, 5 ..]
b) listB = [x | x <- [3, 5 ..]]
c) listC = [x | x <- [3, 5 .. 5999]]
d) listD = [x | x <- [3, 5 .. 6000]]
e) listE = [x | x <- [3, 5 ..], (x < 6000)]
f) listF = [x | x <- takeWhile (< 6000) [3, 5 ..] ]
In other words, for any pair of the preceding lists, list_a and list_b
a) is it true that list_a == list_b? (Why or why not?)
b) will Haskell return True for 
	> list_a == list_b 
-- Why or why not?
-}
primeDivisors n = [d | d <- takeWhile ((<= n) . (^2)) primes, n `mod` d == 0]
primes = 2 : [p | p <- oddsFrom3, null (primeDivisors p)]
-- Approach 1. Look for “k” values.
isPrime g = g == head (dropWhile (< g) primes)
-- Question: How does isPrime work?
-- Generates for any number g the p and k such that g = p + 2 * k * k
goldbachPairs g = [(p, k) | k <- takeWhile ((< g) . (*2) . (^2)) [1 .. ], 
                            p <- [g - 2 * k * k], isPrime p] 
-- Question: What does “p <- [g - 2 * k * k]” do in the preceding?
-- Question:	Explain in your own words what goldbachPairs g contains.
goldbachFails = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                     null (goldbachPairs g) ]
-- Question: What is the list goldbachFails?
-- Approach 2. Look for “p” values.
iSqrt n = floor (sqrt (fromIntegral n))
isSquare n = (iSqrt n) ^ 2 == n 
-- Question: Explain how iSqrt works? 
goldbachPairs' g = [(p, iSqrt kSqr) | p <- takeWhile (< g) (tail primes),  
                                      kSqr <- [(g - p) `div` 2], 
                                      isSquare kSqr]
-- Question: What does “kSqr <- [(g - p) `div` 2]” do in the preceding?
-- Question:	Explain in your own words what goldbachPairs’ g contains.
-- Question:	Why does the search look through (tail primes) rather than primes?
goldbachFails' = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                      null (goldbachPairs' g) ]



--isSquare :: Integral a => a -> Bool
--takes a integer as an argument and returns a boolean



