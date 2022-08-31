module HS20160922 where

-- Given a list of lists, define a function transpose that returns a list containing: a list of all the first elements, 
-- then a list of all the second elements, and so on. Lists can be assumed non empty, but can be of different 
-- lengths. Write all the types of the defined functions.
-- E.g. transpose [[1,2],[3],[4,5,6]]
-- is the list [[1,3,4],[2,5],[6]].

list_ref :: Int -> [a] -> [a]
list_ref _ [] = []
list_ref 1 (x:xs) = [x]
list_ref pos (x:xs) = list_ref (pos-1) xs

list_max :: [Int] -> Int
list_max l = foldr (max) (head l) l

transpose :: [[a]] -> [[a]]
transpose lol = [concatMap (\l -> list_ref i l) lol | i <- [1..(list_max (map (\l -> length l) lol))]]