module HS20220121 where

-- Consider a Tvtl (two-values/two-lists) data structure, which can store either two values of a given type, or
-- two lists of the same type. 
-- Define the Tvtl data structure, and make it an instance of Functor, Foldable, and Applicative.

data Tvtl a = Tv a a | Tl [a] [a] deriving (Show, Eq)

instance Functor Tvtl where
    fmap f (Tv l r) = Tv (f l) (f r)
    fmap f (Tl l r) = Tl (map f l) (map f r)

instance Foldable Tvtl where
    foldr f z (Tv l r) = f l (f r z)
    foldr f z (Tl l r) = foldr f (foldr f z r) l

tvtlconc (Tv lv1 rv1) (Tv lv2 rv2) = Tl ([lv1] ++ [lv2]) ([rv1] ++ [rv2])
tvtlconc (Tv lv rv) (Tl ll rl) = Tl ([lv] ++ ll) ([rv] ++ rl)
tvtlconc (Tl ll rl) (Tv lv rv) = Tl (ll ++ [lv]) (rl ++ [rv])
tvtlconc (Tl ll1 rl1) (Tl ll2 rl2) = Tl (ll1 ++ ll2) (rl1 ++ rl2)

tvtlconcat tvtl = foldr (\x z -> tvtlconc x z) (Tl [] []) tvtl

tvtlconcatmap f tvtl = tvtlconcat $ fmap f tvtl

instance Applicative Tvtl where
    pure x = Tv x x
    fs <*> xs = tvtlconcatmap (\f -> fmap f xs) fs