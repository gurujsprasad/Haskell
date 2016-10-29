doubleAndSum :: [Int] -> Int
doubleAndSum  = fst . foldr (\i (acc, even) -> (acc + nextStep even i, not even)) (0, False)

nextStep :: Bool -> Int -> Int
nextStep even i
 | even      = (uncurryFunc (+) . (`divMod` 10) . (*2)) i
 | otherwise = i 

uncurryFunc :: (x->y->z) ->((x,y) -> z)
uncurryFunc f = \(a,b) -> f a b

myLuhn :: Int -> Bool
myLuhn = ((0 ==) . ((`mod` 10) . (doubleAndSum . ((map (read . (: ""))) . show)))) 
--hidden input
--"445" => ["4","4"]

testCC :: [Bool]
testCC = map myLuhn [49927398661, 36594659453682, 1234567812345678, 1234567812345670]