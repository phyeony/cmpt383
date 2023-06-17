import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- Merging
-- x, y are already sorted lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)  
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys

-- Merge Sort
mergeSort :: (Ord a) => [a] -> [a] 
mergeSort [] = []
mergeSort [x] = [x]
mergeSort n = merge (mergeSort listA) (mergeSort listB)
    where 
        (listA, listB) = split n
        -- Referenced https://stackoverflow.com/questions/19074520/how-to-split-a-list-into-two-in-haskell
        split :: [a] -> ([a],[a])
        split xs = splitAt ((length xs) `div` 2) xs 

-- Haskell Library and Dates
daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday d = snd (mondayStartWeek d) == 5

-- Divisors from exer 2
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

isPrimeDay :: Day -> Bool
isPrimeDay d = divisors (getDay (toGregorian d)) == []
    where 
        getDay :: (Integer, Int, Int) -> Int
        getDay (year, month, day) = day

primeFridays :: Integer -> [Day]
primeFridays y = filter isPrimeAndFriday (daysInYear y)
    where
        isPrimeAndFriday:: Day -> Bool
        isPrimeAndFriday d = isPrimeDay d && isFriday d