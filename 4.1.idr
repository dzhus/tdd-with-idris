module Ex4.1

import Data.Vect

data Tree a = Empty | Node (Tree a) a (Tree a)

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left c right) =
  if x < c then Node (insert x left) c right else Node left c (insert x right)

-- 4.1.1
listToTree : Ord a => List a -> Tree a
listToTree l = listToTree' l Empty
  where
    listToTree' : List a -> Tree a -> Tree a
    listToTree' [] acc = acc
    listToTree' (x :: xs) acc = listToTree' xs (insert x acc)

-- 4.1.2
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left c right) = treeToList left ++ [c] ++ treeToList right

-- 4.1.3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

-- 4.1.4
evaluate : Expr -> Int
evaluate (Val a) = a
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mul a b) = evaluate a * evaluate b

-- 4.1.5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just a) (Just b) = Just (max a b)
maxMaybe Nothing b = b
maxMaybe a Nothing = a

-- vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
-- vectTake Z _ = Nil
-- vectTake (S k) (x :: xs) = x :: vectTake k xs

-- -- sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
-- -- sumEntries p v1 v2 =
-- --   case integerToFin p (length v1) of
-- --     Just pf => Just (index pf v1 + index pf v2)
-- --     Nothing => Nothing

-- Eq Expr where
--   (Val a) == (Val b) = a == b
