module HS20170130 where

-- Define a data structure, called Lt, for generic list of lists, where each list has a fixed length and such 
-- number is stored in the data structure.

data Lt a = Lt Int [[a]] deriving (Eq, Show)

-- Define a function, called checkLt, that takes an Lt and returns true if it is valid (i.e. every list in it have 
-- the correct size), false otherwise.

checkLt :: Lt a -> Bool
checkLt (Lt _ []) = True
checkLt (Lt size (x:xs)) = (size == (length x)) && (checkLt (Lt size xs))

-- Define a function, called checklist, that takes a list t and an Lt, checks if all the sublists of t are in the 
-- given Lt, and uses Maybe to return the list of sublists of t that are not present in Lt.
-- Note: sublists must be contiguous, e.g. the sublists of size 2 of [1,2,3] are [1,2], [2,3].

sublists :: Int -> [a] -> [[a]]
sublists size lst | size > (length lst) = []
sublists size l@(x:xs) = (take size l) : (sublists size xs)

checklist :: (Eq a) => [a] -> Lt a -> Maybe [[a]]
checklist t (Lt size lt) = checklist' (sublists size t) lt [] where
                            checklist' [] lt [] = Nothing
                            checklist' [] lt res = Just res
                            checklist' (x:xs) lt res | not (elem x lt) = checklist' xs lt (x : res)
                            checklist' (x:xs) lt res | elem x lt = checklist' xs lt res

-- Make Lt an instance of Functor.

instance Functor Lt where
    fmap f (Lt n list) = Lt n $ map (\x -> (map f x)) list