module HS20170720 where

-- Define a ternary tree data structure, called Ttree, in which every node contain both a value and a color, which can be either 
-- yellow or blue. You can assume that a tree cannot be empty.

data Color = Blue | Yellow deriving (Eq, Show)

data Ttree a = Leaf Color a | Branch Color a (Ttree a) (Ttree a) (Ttree a) deriving (Eq, Show)

-- Make Ttree an instance of Functor.

instance Functor Ttree where
    fmap f (Leaf c x) = Leaf c (f x)
    fmap f (Branch c x l m r) = Branch c (f x) (fmap f l) (fmap f m) (fmap f r)

-- Make Ttree an instance of Foldable.

instance Foldable Ttree where
    foldr f z (Leaf c x) = f x z
    foldr f z (Branch c x l m r) = foldr f (foldr f (foldr f z r) m) l

-- Define a function yellowSubTrees, which returns a list containing all the maximal subtrees of a given Ttree that are all made of 
-- yellow nodes.

isYellow :: Ttree a -> Bool
isYellow (Leaf c x) = (c == Yellow)
isYellow (Branch c x l m r) = (c == Yellow) && (isYellow l) && (isYellow m) && (isYellow r)

yellowSubTrees self@(Leaf Yellow _) = [self]
yellowSubTrees self@(Leaf Blue _) = []
yellowSubTrees self@(Branch Yellow _ l m r) | (isYellow l) && (isYellow m) && (isYellow r) = [self]
yellowSubTrees (Branch _ _ l m r) = (yellowSubTrees l) ++ (yellowSubTrees m) ++ (yellowSubTrees r)

