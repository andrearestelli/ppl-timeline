module HS20210622 where

-- Define a data-type called BTT which implements trees that can be binary or ternary, and where every 
-- node contains a value, but the empty tree (Nil). Note: there must not be unary nodes, like leaves.

data BTT a = Nil | BNode a (BTT a) (BTT a) | TNode a (BTT a) (BTT a) (BTT a)

-- 1) Make BTT an instance of Functor and Foldable.

instance Functor BTT where
    fmap _ Nil = Nil
    fmap f (BNode v l r) = BNode (f v) (fmap f l) (fmap f r)
    fmap f (TNode v l m r) = TNode (f v) (fmap f l) (fmap f m) (fmap f r)

instance Foldable BTT where
    foldr f z Nil = z
    foldr f z (BNode v l r) = f v $ foldr f (foldr f z r) l
    foldr f z (TNode v l m r) = f v $ foldr f (foldr f (foldr f z r) m) l

-- 2) Define a concatenation for BTT, with the following constraints:
-- • If one of the operands is a binary node, such node must become ternary, and the other operand 
-- will become the added subtree (e.g. if the binary node is the left operand, the rightmost node of 
-- the new ternary node will be the right operand).
-- • If both the operands are ternary nodes, the right operand must be appened on the right of the left 
-- operand, by recursively calling concatenation.

bttconc Nil x = x
bttconc x Nil = x
bttconc (BNode v l r) x = TNode v l r x
bttconc x (BNode v l r) = TNode v x l r
bttconc (TNode v l m r) x = TNode v l m (bttconc r x)

bttconcat btt = foldr (\x y -> bttconc x y) Nil btt

bttconcatmap f btt = bttconcat $ fmap f btt

-- 3) Make BTT an instance of Applicative.

instance Applicative BTT where
    pure x = BNode x Nil Nil
    fs <*> xs = bttconcatmap (\f -> fmap f xs) fs