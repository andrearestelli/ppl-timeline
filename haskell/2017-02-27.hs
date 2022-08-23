module HS20170227 where

-- Consider this data declaration: data LolStream x = LolStream Int [x]. The list [x] must always be an infinite list (also called a 
-- stream), while the first parameter, of type Int, when positive represents the fact that the stream is periodic, while it is not periodic if
-- negative (0 is left unspecified). E.g. these are two valid LolStreams: LolStream -1 [1,2,3...]; LolStream 2 [1,2,1,2,1,2…]

data LolStream x = LolStream Int [x] deriving (Show)

-- Define a function lol2lolstream which takes a finite list of finite lists [h1, h2, … hn], and returns
-- LolStream (|h1| + |h2| + … + |hn|) (h1 ++ h2 ++ … ++ hn ++ h1 ++ h2 ++ …)

buildInfiniteList :: [[x]] -> [x]
buildInfiniteList lst = (foldl (++) [] lst ) ++ (buildInfiniteList lst)

buildPeriod :: [[x]] -> Int
buildPeriod lst = foldl (+) 0 (map length lst)

lol2lolstream :: [[x]] -> LolStream x
lol2lolstream lst = LolStream (buildPeriod lst) (buildInfiniteList lst)

-- Make LolStream an instance of Eq. (Note: == should terminate, when possible.)

-- takes an infinite list and returns the period if n is positive, the infinte list otherwise
stream2l :: Int -> [x] -> [x]
stream2l n stream | n > 0 = take n stream
stream2l _ stream = stream 

-- checks whether two lists are equal, works with infinite lists, even if termination 
-- is not guaranteed
equalsL :: (Eq a) => [a] -> [a] -> Bool
equalsL [] [] = True
equalsL (x:xs) (y:ys) = (x == y) && equalsL xs ys

instance (Eq x) => Eq (LolStream x) where
    (LolStream n1 stream1) == (LolStream n2 stream2) = (n1 == n2) && (equalsL (stream2l n1 stream1) (stream2l n2 stream2))

-- Make LolStream an instance of Functor, Foldable, and Applicative.

instance Functor LolStream where
    fmap f (LolStream n stream) = LolStream n (map f stream)

instance Foldable LolStream where
    foldr f z (LolStream _ []) = z
    foldr f z (LolStream n (x:xs)) = f x (foldr f z xs)

-- exploiting lists' semantics, already instance of Applicative
instance Applicative LolStream where
    pure x = lol2lolstream [[x]]
    (LolStream n1 fs) <*> (LolStream n2 xs) = lol2lolstream [(stream2l n1 fs) <*> (stream2l n2 xs)]

-- Make LolStream an instance of Monad

destream :: (LolStream x) -> [x]
destream (LolStream n xs) = stream2l n xs

-- we can once again exploit the already defined lists' semantics
instance Monad LolStream where
    ls >>= f = lol2lolstream [destream ls >>= (\x -> destream (f x))]

asd = do
    x <- lol2lolstream[[1..4]]
    y <- lol2lolstream[[2..5]]
    return (x,y)

asdList = do
    x <- [1..4]
    y <- [2..5]
    return (x,y)
