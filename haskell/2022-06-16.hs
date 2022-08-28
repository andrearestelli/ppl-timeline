module HS20220616 where

-- Consider the "fancy pair" data type (called Fpair), which encodes a pair of the same type a, and may 
-- optionally have another component of some "showable" type b, e.g. the character '$'. 
-- Define Fpair, parametric with respect to both a and b.

data Fpair b a = Fpair a a | Fpairplus a a b

-- 1) Make Fpair an instance of Show, where the implementation of show of a fancy pair e.g. encoding
-- (x, y, '$') must return the string "[x$y]", where x is the string representation of x and y of y. If the third 
-- component is not available, the standard representation is "[x, y]".

instance (Show a, Show b) => Show (Fpair b a) where
    show (Fpair l r) = "[" ++ show l ++ ", " ++ show r ++ "]"
    show (Fpairplus l r x) = "[" ++ show l ++ show x ++ show r ++ "]"

-- 2) Make Fpair an instance of Eq — of course the component of type b does not influence the actual 
-- value, being only part of the representation, so pairs with different representations could be equal.

instance (Eq a) => Eq (Fpair b a) where
    (Fpair l1 r1) == (Fpair l2 r2) = (l1 == l2) && (r1 == r2)
    (Fpairplus l1 r1 _) == (Fpair l2 r2) = (l1 == l2) && (r1 == r2)
    (Fpair l1 r1) == (Fpairplus l2 r2 _) = (l1 == l2) && (r1 == r2)
    (Fpairplus l1 r1 _) == (Fpairplus l2 r2 _) = (l1 == l2) && (r1 == r2)

-- 3) Make Fpair an instance of Functor, Applicative and Foldable.

instance Functor (Fpair b) where
    fmap f (Fpair l r) = Fpair (f l) (f r)
    fmap f (Fpairplus l r s) = Fpairplus (f l) (f r) s

instance Foldable (Fpair b) where
    foldr f z (Fpair l r) = f l (f r z)
    foldr f z (Fpairplus l r _) = f l (f r z)

instance Applicative (Fpair b) where
    pure x = Fpair x x
    (Fpair lf rf) <*> (Fpair lx rx) = Fpair (lf lx) (rf rx)
    (Fpairplus lf rf s) <*> (Fpair lx rx) = Fpairplus (lf lx) (rf rx) s
    (Fpair lf rf) <*> (Fpairplus lx rx s) = Fpairplus (lf lx) (rf rx) s
    (Fpairplus lf rf s) <*> (Fpairplus lx rx v) = Fpairplus (lf lx) (rf rx) v