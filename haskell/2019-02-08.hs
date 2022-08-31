module HS20190208 where

data BFlist a = Flist [a] | Blist [a] 

instance (Eq a) => Eq (BFlist a) where
    Flist ls == Flist rs = ls == rs
    Blist ls == Blist rs = ls == rs
    Flist ls == Blist rs = ls == (reverse rs)
    Blist ls == Flist rs = ls == (reverse rs)

instance (Show a) => Show (BFlist a) where
    show (Flist ls) = "+" ++ show ls
    show (Blist ls) = "-" ++ show ls

(Blist ls) <++> (Blist rs) = Blist (ls ++ rs)
(Flist ls) <++> (Flist rs) = Flist (ls ++ rs)
(Blist ls) <++> (Flist rs) = Flist ((reverse ls) ++ rs)
(Flist ls) <++> (Blist rs) = Flist (ls ++ (reverse rs))

instance Functor BFlist where
    fmap f (Flist ls) = Flist (fmap f ls)
    fmap f (Blist ls) = Blist (fmap f ls)

instance Foldable BFlist where
    foldr f z (Flist ls) = foldr f z ls
    foldr f z (Blist ls) = foldr f z (reverse ls)

bfconcat ls = foldr (<++>) (Flist []) ls
bfconcatmap f ls = bfconcat $ fmap f ls

instance Applicative BFlist where
    pure x = (Flist [x])
    fs <*> xs = bfconcatmap (\f -> fmap f xs) fs