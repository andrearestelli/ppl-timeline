module HS20170705 where

data Tree a = Nil | Leaf a | Branch (Tree a)(Tree a) deriving (Show, Eq)

-- Define a tcompose operation, which takes a function f and two trees, t1 and t2, and returns a tree with the same structure as t1, 
-- but with leaves replaced by subtrees having the same structure of t2: each leaf is obtained by applying f to the value stored in the 
-- previous leaf, and the corresponding value in t2.
-- e.g. t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3), t2 = Branch (Leaf 6) (Leaf 7)
-- tcompose (+) t1 t2 is 
-- Branch (Branch (Branch (Leaf 7) (Leaf 8)) (Branch (Leaf 8) (Leaf 9))) (Branch (Leaf 9) (Leaf 10))

buildSubTree :: Tree a -> (a -> a -> a) -> a -> Tree a
buildSubTree Nil f value = Nil
buildSubTree (Leaf x) f value = Leaf $ f x value
buildSubTree (Branch l r) f value = Branch (buildSubTree l f value) (buildSubTree r f value)

tcompose :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
tcompose f Nil t2 = Nil
tcompose f (Leaf x) t2 = buildSubTree t2 f x
tcompose f (Branch l r) t2 = Branch (tcompose f l t2) (tcompose f r t2)

-- Define a purely functional, non destructive, reverse which takes a binary tree and keeps its structure, but “reversing” all the values in 
-- the leaves. E.g. (revtree t1) is the tree Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1).

treeToListReversed :: Tree a -> [a]
treeToListReversed Nil = []
treeToListReversed (Leaf x) = [x]
treeToListReversed (Branch l r) = (treeToListReversed r) ++ (treeToListReversed l)

-- exploit pair to return both the tree built and the list to take the elements from
revtree t = t1
    where (t1, _) = revtree' t (treeToListReversed t) where
                        revtree' Nil xs = (Nil, xs)
                        revtree' (Leaf v) (x:xs) = (Leaf x, xs)
                        revtree' (Branch l r) xs = let (l', xs') = revtree' l xs
                                                       (r', xs'') = revtree' r xs'
                                                   in (Branch l' r', xs'')
