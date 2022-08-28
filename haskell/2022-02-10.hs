module HS20220210 where

-- Consider a data structure Gtree for general trees, i.e. trees containg some data in each node, and a 
-- variable number of children.
-- 1. Define the Gtree data structure.

data Gtree a = Nil | Node a [Gtree a] deriving (Show)

-- 2. Define gtree2list, i.e. a function which translates a Gtree to a list.

gtree2list Nil = []
gtree2list (Node v ls) = [v] ++ concatMap gtree2list ls

-- 3. Make Gtree an instance of Functor, Foldable, and Applicative.

instance Functor Gtree where
    fmap f Nil = Nil
    fmap f (Node v ls) = Node (f v) (map (\gt -> fmap f gt) ls)

instance Foldable Gtree where
    foldr f z gt = foldr f z (gtree2list gt)

gtconc Nil gt = gt
gtconc (Node v ls) gt = Node v (ls ++ [gt])

gtconcat gt = foldr (\x y -> gtconc x y) Nil gt
gtconcatmap f gt = gtconcat $ fmap f gt

instance Applicative Gtree where
    pure x = Node x []
    fs <*> xs = gtconcatmap (\f -> fmap f xs) fs