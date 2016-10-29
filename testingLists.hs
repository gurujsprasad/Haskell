t = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ] --this is for generating a triangle

--lets create a function with a type declaration
removeLowerCase :: [Char] -> [Char]
removeLowerCase st = [c | c <- st, c `elem` ['A'..'Z']]

--Functions with multiple Arguments and Type declartion
addThreeNumbers :: Int -> Int -> Int -> Int
addThreeNumbers x y z = x + y + z

--Function to find factorial of n numbers , her use Integer because it doesnt have any bound
factorialOfN :: Integer -> Integer
factorialOfN n = product [1..n]

--Floating point type declaration
floatingPointProduct :: Float -> Float -> Float
floatingPointProduct x y = x * y

checkList :: [Int] -> [Int]
checkList ls = init ls

--returning the first tuple
returnFirstTuple a = fst a