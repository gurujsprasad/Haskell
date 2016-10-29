import Data.Char (digitToInt)

digitSum :: Int -> Int
digitSum num = (div num 10) + (mod num 10)

checkSum :: [Int] -> [Int]
checkSum num = map digitSum (zipWith (*) num (cycle[1,2]) )

toIntList :: [Char] -> [Int]
toIntList number = reverse(map digitToInt number)
--"467" => [4,6,7]

isValidCard :: [Char] -> Bool
isValidCard number = (sum . checkSum . toIntList) number `mod` 10 == 0

--u need to double all the elements from the right starting from second position and all the alternative
--u will all the double 2 digit elemenents in to single digit
--u will do sum of all the digits
--u will mod 10 the sum 
--if its ==0 then valid
	--else not valid