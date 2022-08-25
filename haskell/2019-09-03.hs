module HS20190903 where

-- Consider the data structure Tril, which is a generic container consisting of three lists.
-- 1) Give a data definition for Tril.

data Tril a = Tril [a] [a] [a] deriving (Show, Eq)

-- 2) Define list2tril, a function which takes a list and 2 values x and y, say x < y, and builds a Tril, where 
-- the last component is the ending sublist of length x, and the middle component is the middle sublist of 
-- length y-x. Also, list2tril L x y = list2tril L y x.
-- E.g. list2tril [1,2,3,4,5,6] 1 3 should be a Tril with first component [1,2,3], second component [4,5], and 
-- third component [6].

takeRange l 0 num = take num l
takeRange (x:xs) from num = takeRange xs (from-1) num

list2tril l x y = Tril (takeRange l 0 ((length l)-y)) (takeRange l ((length l)-y) (y-x)) (takeRange l ((length l)-x) x)

-- 3) Make Tril an instance of Functor and Foldable.

instance Functor Tril where
    fmap f (Tril l m r) = Tril (map f l) (map f m) (map f r)

instance Foldable Tril where
    foldr f z (Tril l m r) = foldr f (foldr f (foldr f z r) m) l

-- 4) Make Tril an instance of Applicative, knowing that the concatenation of 2 Trils has first component 
-- which is the concatenation of the first two components of the first Tril, while the second component is the
-- concatenation of the ending component of the first Tril and the beginning one of the second Tril (the third
-- component should be clear at this point).

-- we have to define conc, concat, concatMap
triconc (Tril l1 m1 r1) (Tril l2 m2 r2) = Tril (l1 ++ m1) (r1 ++ l2) (m2 ++ r2)
triconcat t = foldr (triconc) (Tril [] [] []) t
triconcatmap f t = triconcat $ fmap f t

instance Applicative Tril where
    pure x = Tril [x] [] []
    fs <*> xs = triconcatmap (\f -> fmap f xs) fs

