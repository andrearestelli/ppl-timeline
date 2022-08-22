module HS20150922 where

-- Define the Bilist data-type, which is a container of two homogeneous lists. Define an accessor for Blist,
-- called bilist_ref, that, given an index i, returns the pair of values at position i in both lists.
-- e.g. bilist_ref (Bilist [1,2,3] [4,5,6]) 1 should return (2,5).

data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref (Bilist l r) i = (l !! i , r !! i)

-- Define a function, called oddeven, that is used to build a Bilist x y from a simple list. oddeven takes all
-- the elements at odd positions and put them in y, while all the other elements are put in x, maintaining their
-- order. You may assume that the given list has an even length (or 0). Write also all the types of the functions
-- you define.
-- e.g. oddeven [1,2,3,4] must be Bilist [1,3] [2,4].

oddeven :: [a] -> Bilist a
oddeven lst = oddeven' lst 0 [] [] where
                oddeven' :: [a] -> Int -> [a] -> [a] -> Bilist a
                oddeven' [] i resL resR = Bilist resL resR 
                oddeven' lst i resL resR | i >= (length lst) = Bilist resL resR
                oddeven' lst i resL resR | even i = oddeven' lst (i + 1) (resL ++ [(lst !! i)]) resR
                oddeven' lst i resL resR | odd i = oddeven' lst (i + 1) resL (resR ++ [(lst !! i)])

-- Define an inverse of oddeven, e.g. inv_oddeven $ oddeven [1,2,3,4] must be [1,2,3,4]. Write also all
-- the types of the functions you define.

inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist [] []) = []
inv_oddeven (Bilist (x:xs) rl) = [x] ++ (inv_oddeven (Bilist rl xs))

-- Define a function, called bilist_max, that given an input Bilist [x1, x2, . . . , xn] [y1, y2, . . . , yn], where xk +
-- yk, for 1 ≤ k ≤ n, is the maximum, returns k.
-- E.g.
-- > bilist_max (Bilist [3,2,-1] [2,1,7])
-- 2

bilist_max :: Bilist Int -> Int
bilist_max (Bilist l r) = find_index_max (map (\(x,y) -> x+y) $ zip l r) 0 ((head l) + (head r)) 0 where 
                                find_index_max lst i curr_max res | i >= (length lst) = res
                                find_index_max lst i curr_max res | (lst !! i) > curr_max = find_index_max lst (i + 1) (lst !! i) i
                                find_index_max lst i curr_max res | (lst !! i) <= curr_max = find_index_max lst (i + 1) curr_max res