arabic_num :: String -> (Int,String)
arabic_num ('M':rest) = (1000, rest)
arabic_num ('C':'M':rest) = (900, rest)
arabic_num ('D':rest) = (500, rest)
arabic_num ('C':'D':rest) = (400, rest)
arabic_num ('C':rest) = (100, rest)
arabic_num ('X':'C':rest) = (90, rest)
arabic_num ('L':rest) = (50, rest)
arabic_num ('X':'L':rest) = (40, rest)
arabic_num ('X':rest) = (10, rest)
arabic_num ('I':'X':rest) = (9, rest)
arabic_num ('V':rest) = (5, rest)
arabic_num ('I':'V':rest) = (4, rest)
arabic_num ('I':rest) = (1, rest)


testCases = ["MCMXC", "MMVIII", "MDCLXVI"]

test = zip testCases (map toArabic testCases)  

toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic rest 
        where (num, rest) = arabic_num str