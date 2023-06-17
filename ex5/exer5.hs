import Data.Ratio
import Data.List
-- Built-in Functions
myIterate :: (a->a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = (takeN n xs, dropN n xs)
    where
        dropN :: Int -> [a] -> [a]
        dropN _ [] = []
        dropN 0 xs = xs
        dropN n (x:xs) = drop (n-1) xs
        takeN :: Int -> [a] -> [a]
        takeN _ [] = []
        takeN 0 xs = []
        takeN n (x:xs) = x : takeN (n-1) xs

-- Rational Numbers 
rationalSum :: Int -> [Ratio Int]
rationalSum n = [ i%(n-i) | i <- [1..(n-1)]]

rationalSumLowest :: Int -> [Ratio Int]
rationalSumLowest n = [ i%(n-i) | i <- [1..(n-1)], gcd i (n-i) == 1]

-- All Rational Numbers
rationals :: [Ratio Int]
-- rationals' = foldl (\acc x -> acc ++ rationalSumLowest x) [] [1..] -- Gets stuck. WHy???
-- rationals''' = foldl' (\acc x -> acc ++ rationalSumLowest x) [] [1..] -- Gets stuck.
-- rationals'' = foldr (\x acc -> rationalSumLowest x ++ acc) [] [1..] -- WOrks.. WHy??
rationals = rationalSumLowestAcc [] 1 -- works.
    where rationalSumLowestAcc xs acc = xs ++ rationalSumLowest (acc) ++ rationalSumLowestAcc xs (acc+1)

-- -- Input/Output
-- split a list around a given separator value
splitAtSeparator :: Eq a => a -> [a] -> [[a]]
splitAtSeparator sep [] = []
splitAtSeparator sep content = first : splitAtSeparator sep rest
    where
    first = takeWhile (/= sep) content
    firstlen = length first
    rest = drop (firstlen+1) content

-- convert an integer-like string to an integer
readInt :: String -> Int
readInt = read

sumFile :: IO ()
sumFile = do 
    contents <- readFile "input.txt"
    let contentArray = splitAtSeparator '\n' contents
    print $ foldl (\acc x -> acc + readInt x) 0 contentArray