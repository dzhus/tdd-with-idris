module Ex415

import Data.Vect

data Tree a = Empty | Node (Tree a) a (Tree a)

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left c right) =
  if x < c then Node (insert x left) c right else Node left c (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree l = listToTree' l Empty
  where
    listToTree' : List a -> Tree a -> Tree a
    listToTree' [] acc = acc
    listToTree' (x :: xs) acc = listToTree' xs (insert x acc)

treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left c right) = treeToList left ++ [c] ++ treeToList right

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

eval : Expr -> Int
eval (Val a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
-- eval (f a b) = (ConToFun f) (eval a) (eval b) --

-- conToFun : (Expr -> Expr -> Expr) -> (Int -> Int -> Int)
-- conToFun Add = (+)
-- conToFun Sub = (-)
-- conToFun Mul = (*)

-- maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
-- maxMaybe (Just a) (Just b) = Just (max a b)
-- maxMaybe Nothing (Just b) = b

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z _ = Nil
vectTake (S k) (x :: xs) = x :: vectTake k xs

-- sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
-- sumEntries p v1 v2 =
--   case integerToFin p (length v1) of
--     Just pf => Just (index pf v1 + index pf v2)
--     Nothing => Nothing

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (Triangle a1 h1) == (Triangle a2 h2) = a1 == a2 && h1 == h2
  (Rectangle a1 b1) == (Rectangle a2 b2) = a1 == a2 && b1 == b2
  (Circle a) == (Circle b) = a == b
  a == b = False

area : Shape -> Double
area (Circle r) = pi * r * r
area (Triangle a h) = a * h / 2
area (Rectangle a b) = a * b

Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)


Eq Expr where
  (Val a) == (Val b) = a == b
