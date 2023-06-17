-- Pascal's Triangle
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = [1] ++ map (uncurry (+)) (zip prev (tail prev)) ++ [1]
    where 
        prev = pascal (n-1) 

-- Pointfree Addition
addPair :: (Num a) => (a,a) -> a 
addPair = (uncurry (+))

-- Pointfree Filtering
withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter (\x -> x /= 0)

-- Searching? Maybe?
-- findElt :: (Maybe m, Eq a) => a -> [a] -> m a
-- findElt x, (y: ys) =
--     case x==y of
--         Nothing -> findElt x, ys
--         Just x -> 
findElt :: (Eq a, Num b) => a -> [a] -> Maybe b
findElt x ys = findEltIdx x ys 0
    where 
        findEltIdx :: (Eq a, Num b) => a -> [a] -> b -> Maybe b
        findEltIdx x [] idx = Nothing
        findEltIdx x (y:ys) idx = 
            case x == y of
                False -> findEltIdx x ys (idx+1)
                True -> Just idx
