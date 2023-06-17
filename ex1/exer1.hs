det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c))/2*a
quadsol2 a b c = (-b + sqrt (det a b c))/2*a

-- third element
third_a xs = xs !! 2
third_b (_:_:x:_) = x

-- Factorial
fact 0 = 1
fact x = (x) * fact (x-1)

fact2 a 0 = a
fact2 a x =  fact2 (a*x) (x-1)

-- Hailstone Func
hailstone n
    | even n = n `div` 2
    | odd n = 3*n + 1

-- Hailstone Length
hailLen 1 = 0
hailLen n = hailLen(hailstone n) + 1