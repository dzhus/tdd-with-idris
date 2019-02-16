module Ex8.2

import Data.Vect

-- 8.2.1
total myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k

-- 8.2.2
total myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' {n} {m = (S len)} acc (x :: xs) =
          rewrite sym (plusSuccRightSucc n len) in reverse' (x :: acc) xs
