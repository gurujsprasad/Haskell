increasing :: (Ord a) => [a] -> Bool
increasing ls = if ls == []
				then True
				else if tail ls == []
					then True
					else if head ls <= head (tail ls)
						then increasing (tail ls)
						else False

checkIfItsSeven :: (Integral a) => a -> String
checkIfItsSeven 7 = "Yes its seven !!"
checkIfItsSeven x = "nope not the one !!"

luckySeven :: (Integral a) => a -> Bool
luckySeven x
	| x ==7 = True
	| otherwise = False 

factorialOfNumbers :: (Integral a) => a -> a
factorialOfNumbers 0 = 1
factorialOfNumbers n = n * factorialOfNumbers (n-1)

extractTupleInfo :: (Num a) => (a,a) -> (a,a) -> [a]
extractTupleInfo a b = fst a: snd a: fst b: snd b:[]

returnThirdvalue :: (Num a) => (a,a,a) -> a
returnThirdvalue (_,_,x) = x

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

sumOfList :: (Num b) => [b] -> b
sumOfList [] = 0
sumOfList [x] = x
sumOfList xs@(x:ls) = x + sumOfList ls

printFirstWord ::String -> String
printFirstWord "" = "String is empty !!"
printFirstWord all@(x:xs) = "first letter of "++all++" is : "++[x]