module HS20210831 where

-- Consider a Slist data structure for lists that store their length. Define the Slist data structure, and make it
-- an instance of Foldable, Functor, Applicative and Monad.

data Slist a = Slist Int [a] deriving (Show, Eq)

instance Foldable Slist where
  foldr f z (Slist l xs) = foldr f z xs

instance Functor Slist where
  fmap f (Slist l xs) = Slist l (fmap f xs)

(Slist l1 xs1) <++> (Slist l2 xs2) = Slist (l1 + l2) (xs1 ++ xs2)

sconcat l = foldr (<++>) (Slist 0 []) l
sconcatmap f l = sconcat $ fmap f l

instance Applicative Slist where
  pure x = Slist 1 [x]
  fs <*> xs = sconcatmap (\f -> fmap f xs) fs

instance Monad Slist where
  xs >>= f = sconcatmap f xs
