import Data.List (isPrefixOf)

mapping = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]

toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic rest
               where (num, rest) = oneStep str


oneStep :: String  -> (Int, String)
oneStep str = [(num, drop (length roman) str) 
              
              |(roman,num) <- mapping, roman `isPrefixOf` str]


testCases = ["MCMXC", "MMVIII", "MDCLXVI"]


test = zip testCases (map toArabic testCases) 