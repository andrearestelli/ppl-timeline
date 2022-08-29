module HS20220706 where

-- A deque, short for double-ended queue, is a list-like data structure that supports efficient element 
-- insertion and removal from both its head and its tail. Recall that Haskell lists, however, only support O(1)
-- insertion and removal from their head.
-- Implement a deque data type in Haskell by using two lists: the first one containing elements from the 
-- initial part of the list, and the second one containing elements from the final part of the list, reversed.
-- In this way, elements can be inserted/removed from the first list when pushing to/popping the deque's 
-- head, and from the second list when pushing to/popping the deque's tail.
-- 1) Write a data type declaration for Deque.

data Deque a = Deque [a] [a]

-- 2) Implement the following functions:
-- • toList: takes a Deque and converts it to a list

reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

toList (Deque l r) = l ++ (reverseList r)

-- • fromList: takes a list and converts it to a Deque

fromList l = let half = (length l) `div` 2
             in Deque (take half l) (take ((length l)-half) (reverseList l))

-- • pushFront: pushes a new element to a Deque's head

pushFront x (Deque front back) = Deque (x:front) back

-- • popFront: pops the first element of a Deque, returning a tuple with the popped element and the 
-- new Deque

popFront (Deque (x:front) back) = (x, Deque front back)

-- • pushBack: pushes a new element to the end of a Deque

pushBack x (Deque front back) = Deque front (x:back)

-- • popBack: pops the last element of a Deque, returning a tuple with the popped element and the new
-- Deque

popBack (Deque front (x:back)) = (x, Deque front back)

-- 3) Make Deque an instance of Eq and Show.

instance (Eq a) => Eq (Deque a) where
    x == y = (toList x) == (toList y)

instance Show a => Show (Deque a) where
    show x = show (toList x)

-- 4) Make Deque an instance of Functor, Foldable, Applicative and Monad.
-- You may rely on instances of the above classes for plain lists.

-- relying on Functor instance for plain lists
instance Functor Deque where
    fmap f (Deque front back) = Deque (fmap f front) (fmap f back)

-- relying on Foldable instance for plain lists
instance Foldable Deque where
    foldr f z x = foldr f z (toList x)

instance Applicative Deque where
    pure x = Deque [x] []
    fs <*> xs = fromList ((toList fs) <*> (toList xs))

dconc x y = fromList $ (toList x) ++ (toList y)
dconcat l = foldr (\x y -> dconc x y) (Deque [] []) l
dconcatMap f x = dconcat $ fmap f x

instance Monad Deque where
    xs >>= f = dconcatMap f xs