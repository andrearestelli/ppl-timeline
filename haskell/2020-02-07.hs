module HS20200207 where

-- Consider a data type PriceList that represents a list of items, where each item is associated with a price, 
-- of type Float:

data PriceList a = PriceList [(a, Float)]
-- 1) Make PriceList an instance of Functor and Foldable.

pmap f z (PriceList ls) = PriceList (map (\x -> (f (fst x), (snd x) + z)) ls)

instance Functor PriceList where
    fmap f pc = pmap f 0.0 pc

instance Foldable PriceList where
    foldr f z (PriceList ls) = foldr (\x y -> (f (fst x) y)) z ls

-- 2) Make PriceList an instance of Applicative, with the constraint that each application of a function in the
-- left hand side of a <*> must increment a right hand side value’s price by the price associated with the 
-- function.

plconc (PriceList l1) (PriceList l2) = PriceList (l1 ++ l2)
plconcat pl = foldr (\x y -> plconc x y) (PriceList []) pl
plconcatMap f pl = plconcat $ fmap f pl

instance Applicative PriceList where
    pure x = PriceList [(x, 0.0)]
    (PriceList fs) <*> pls = plconcatMap (\ff -> let (f, v) = ff
                                                 in pmap f v pls) fs
