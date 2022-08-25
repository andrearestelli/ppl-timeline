module HS20190628 where

-- 1) Define a Tritree data structure, i.e. a tree where each node has at most 3 children, and every node contains 
-- a value.

data Tritree a = Nil | Node a (Tritree a) (Tritree a) (Tritree a) deriving (Show, Eq)

-- 2) Make Tritree an instance of Foldable and Functor.

instance Foldable Tritree where
    foldr f z Nil = z
    foldr f z (Node x l m r) = foldr f (foldr f (foldr f (f x z) r) m) l

instance Functor Tritree where
    fmap f Nil = Nil
    fmap f (Node a l m r) = Node (f a) (fmap f l) (fmap f m) (fmap f r)
 
-- 3) Define a Tritree concatenation t1 +++ t2, where t2 is appended at the bottom-rightmost position of t1.

(+++) :: Tritree a -> Tritree a -> Tritree a
t1 +++ Nil = t1
Nil +++ t1 = t1
(Node x l m r) +++ t2 = Node x l m (r +++ t2)

-- 4) Make Tritree an instance of Applicative.

-- we have to define concat and concatMap for Tritrees
triconcat tl = foldr (+++) Nil tl
triconcmap f t = triconcat $ fmap f t

instance Applicative Tritree where
    pure x = Node x Nil Nil Nil
    fs <*> ts = triconcmap (\f -> fmap f ts) fs