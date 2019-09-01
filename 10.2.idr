module Ex10.2

import Data.List.Views
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views

-- 10.2.1
total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty with (snocList ys)
    equalSuffix [] [] | Empty | Empty = []
    equalSuffix [] (xs ++ [x]) | Empty | (Snoc rec) = []
  equalSuffix (xprefix ++ [x]) ys | (Snoc xrec) with (snocList ys)
    equalSuffix (xprefix ++ [x]) [] | (Snoc xrec) | Empty = []
    equalSuffix (xprefix ++ [x]) (yprefix ++ [y]) | (Snoc xrec) | (Snoc yrec) =
      if x == y
      then (equalSuffix xprefix yprefix | xrec | yrec) ++ [x]
      else []

-- 10.2.2
total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | (SplitRecPair lrec rrec) =
    merge (mergeSort ys | lrec) (mergeSort zs | rrec)

-- 10.2.3
total
toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = (toBinary n | rec) ++ "1"

-- 10.2.4
total
palindrome : Eq a => List a -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = x == y && (palindrome xs | rec)
