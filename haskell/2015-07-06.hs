module HS20150706 where

producer ag1 ag2 = prod' ag1 [] ag2 0 1
    where
        prod' :: (Int -> [Int] -> [Int]) -> [Int] -> (Int -> Int -> Int) -> Int -> Int -> [Int]
        prod' ag1 st1 ag2 st2 i | i >= 10 = (st2:st1)
        prod' ag1 st1 ag2 st2 i | odd i = prod' ag1 (ag1 i st1) ag2 st2 (i+1)
        prod' ag1 st1 ag2 st2 i | even i = prod' ag1 st1 ag2 (ag2 i st2) (i+1)

sumprefix x y = x + y

consprefix x y = x : y

-- Define an higher-order function called duofold, which takes two binary functions f and g, a starting value t
-- and a (finite) list [e1, e2, . . .], and returns . . . f(g(f(t, e1), e2), e3), .... Please, write also its type.
-- Example: duofold (+) (-) 0 [1,2,3,4] returns −2 (i.e. 0 + 1 − 2 + 3 − 4).

duofold :: (a -> a -> a) -> (a -> a -> a) -> a -> [a] -> a
duofold f g t [] = t
duofold f g t (x:y:xs) = duofold f g (g (f t x) y) xs
duofold f g t (x:[]) = f t x

-- more elegant
duofoldv2 f g v [] = v
duofoldv2 f g k (x:xs) = duofoldv2 g f (f k x) xs