module HS20150910 where

-- Define a class called Blup, for a generic type T having two parameters x and y, providing two operations
-- called f isto and fosto. f isto takes a value belonging to T and returns a value of type M aybe x, while fosto
-- takes a value belonging to T and returns a value of type M aybe y.

class Blup a where
    fisto :: (a b c) -> Maybe b
    fosto :: (a b c) -> Maybe c

-- Define the sum type Blargh with two parameters of types a and b. It has three data constructor: either Bip
-- with two parameters of types respectively a and b, or Bop with only one parameter of type a, or Bup with no
-- parameters.
-- Make Blargh an instance of class Blup, where f isto is used to access to data of type a, and fosto to
-- data of type b.

data Blargh a b = Bip a b | Bop a | Bup deriving (Show, Eq)

instance Blup Blargh where
    fisto (Bup) = Nothing
    fisto (Bop x) = Just x
    fisto (Bip x y) = Just x
    fosto (Bup) = Nothing
    fosto (Bop x) = Nothing
    fosto (Bip x y) = Just y

-- Define the sum type Blarf with two parameters of types a and b. It has two data constructor: either La and
-- a list of elements of type a, or Lb and a list of elements of type b.
-- Make Blarf an instance of class Blup, where f isto is used to access to the head of the list of elements
-- of type a, and fosto to the head of the list of elements of type b.

data Blarf a b = La [a] | Lb [b] deriving (Show, Eq)

instance Blup Blarf where
    fisto (La (x:xs)) = Just x
    fisto _ = Nothing
    fosto (Lb (x:xs)) = Just x
    fosto _ = Nothing

-- Define a function smap that takes an infinite list L of Int, a function f from Int to Int, an operation OP
-- over Int, and a threshold T. smap performs a map of f on L, while keeping an accumulator K (with
-- starting value 0), which is updated at each step as oldAccumulatorV alue OP f(currentElementOfL).
-- smap stops when the value of K reaches T and returns a list of all the computed values of the map.
-- e.g. smap (^2) (+) [1,2..] 100 is the list [1,4,9,16,25,36,49].

smap :: (Int -> Int) -> (Int -> Int -> Int) -> [Int] -> Int -> [Int]
smap f op list threshold = smap' op threshold (map f list) 0 where
                                smap' op threshold list acc | acc >= threshold = []
                                smap' op threshold (x:xs) acc = x : smap' op threshold xs (op acc x)

