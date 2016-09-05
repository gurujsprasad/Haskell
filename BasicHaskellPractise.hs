doubleMe y = y + y

multiplication x z = x*z

firstList = 99:[1,2 .. 15] ++ [1,3 .. 19]

lengthOfFirstList = length firstList

getListByIndex = [1,2,3,4] !! 4

appendingList = [[1,2,3,5],[3,5,6]]

multipleList = [[2,4 .. 8],[1,3 .. 9]] ++ appendingList


getThirdList = multipleList !! 2

addAndMultiply x z = doubleMe x + multiplication x z

squareIFAboveHundred x = if x > 100
							then sqrt (x)
							else x/32

guruprasad' = "Its me Guruprasad J S" ++ ". Am an idiot and lazy bitch"

listComprehension =  [x*1.5 | x <- [1 .. 15]]

listComprehensionCondition = [x*2 | x <- [1..5], x >= 3]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]



