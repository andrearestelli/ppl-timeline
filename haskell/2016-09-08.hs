module HS20160908 where

checkRow :: Int -> Int -> Int -> [Int] -> Bool
checkRow dim reached _ [] = (dim == reached)
checkRow dim reached 0 (x:xs) = (x == 1) && (checkRow dim (reached + 1) (-1) xs)
checkRow dim reached pos (x:xs) = (x == 0) && (checkRow dim (reached + 1) (pos-1) xs)

checkCol :: Int -> Int -> [[Int]] -> Bool
checkCol _ _ [] = True
checkCol dim pos (x:xs) = (checkRow dim 0 pos x) && (checkCol dim (pos + 1) xs)

checkFig :: [[Int]] -> Maybe Int
checkFig l | checkCol (length l) 0 l = Just (length l)
checkFig l = Nothing
