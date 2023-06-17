-- Primes and Divisors
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]
primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

-- Pythagorean Triples
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a, b, c)| c <- [1..n], a<-[1..n], b <-[1..n], a^2+b^2==c^2, a<b, b<c]

-- -- Joining Strings
join :: String -> [String] -> String
join _ [] = ""
join _ [y] = y
join x (y:ys) = y ++ x ++ join x ys

-- Factorial with a fold
-- fact' :: Int -> Int
fact' n = foldl (*) 1 [1..n]

-- Tail Recursive Hailstone
hailstone n
    | even n = n `div` 2
    | odd n = 3*n + 1

hailLen n = hailTail 0 n
  where
    hailTail a 1 = a
    hailTail a x = hailTail (a+1) (hailstone x)