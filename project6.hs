import Data.List
import Data.Ord

concatNum :: Int -> Int -> Int
concatNum x y = read ((show x) ++ (show y))

myCompare :: Int -> Int -> Ordering
myCompare x y = flip compare (concatNum x y) (concatNum y x)

mydot ::  (b -> c) -> (a -> b) -> a -> c
mydot f g = \x -> f (g x)

largestNumber :: [Int] -> Integer
largestNumber =  read `mydot` (concat `mydot` (map show `mydot` sortBy myCompare))