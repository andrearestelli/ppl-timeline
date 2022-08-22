module HS20150724 where

-- Make lists of numbers a instances of the class Num. If the two lists have different length, 
-- you must assume that the missing elements of the shorter are all 0.
-- e.g. [1,2,3] * [2,-1] should be [2,-2,0].
-- (Remember that you need to define methods for +, -, *, abs, signum, and fromInteger.)

instance (Num f) => (Num [f]) where
    x + [] = x
    [] + x = x
    (x:xs) + (y:ys) = (x+y) : (xs+ys)
    x - [] = x
    [] - x = -x
    (x:xs) - (y:ys) = (x-y) : (xs-ys)
    x * [] = map (\x -> 0) x
    [] * x = map (\x -> 0) x
    (x:xs) * (y:ys) = (x*y) : (xs*ys)
    abs = map abs
    signum = map signum
    fromInteger x = [fromInteger x]

-- Define a recursive data structure of type TT which can be used to represent lists of Int of any depth (e.g. in
-- Scheme ’(1 2 (3 9) ((1) -7))).

data TT = VV Int | LL [TT] deriving (Eq, Show)

-- Define a predicate lile, which, given a TT value, check if it contains its own length.
-- e.g., using a Scheme-like notation (lile ’(2 1)) holds, while (lile ’(1 2 1)) does not.

member x (LL y) = elem (VV x) y

len (VV _) = 0
len (LL x) = length x

-- true if the list contains its own length in its higher level
lile (LL []) = True
lile y = member (len y) y

-- helper function used to check that all the sublists of a list 
-- contains their own length
elemTTg (LL []) = True
elemTTg (LL ((VV x):xs)) = (elemTTg (LL xs))
elemTTg (LL (x:xs)) = lile x && (elemTTg (LL xs))

lileg t = lile t && elemTTg t