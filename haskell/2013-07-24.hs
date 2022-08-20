module HS20130724 where

data DList a = Nil | Node (DList a) a (DList a)   

instance Show a => Show (DList a) where
  show Nil = "Nil"
  show (Node prev a next) = show a ++ " -> " ++ show next

instance (Eq a) => Eq (DList a) where
    Nil == Nil = True
    (Node preva a nexta) == (Node prevb b nextb) = (a == b) && (nexta == nextb) 
    _ == _ = False

car :: DList a -> Maybe a
car Nil = Nothing
car (Node prev a next) = Just a

cdr :: DList a -> Maybe (DList a)
cdr Nil = Nothing
cdr (Node prev a next) = Just next

cons :: a -> DList a -> DList a
cons x Nil = Node Nil x Nil
-- recursive definition of new, exploiting call by need
cons x (Node Nil b nextb) = let new = (Node Nil x (Node new b nextb))
                            in new