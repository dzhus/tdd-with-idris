module Ex8.3

import Data.Vect

-- 8.3.1
total headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
            (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

total tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
            (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- 8.3.2
data MyVect : Nat -> a -> Type where
  Nil  : MyVect 0 a
  (::) : a -> MyVect n a -> MyVect (S n) a

unequalHeads : {xs : MyVect a n} -> {ys : MyVect a n} ->
          (contra : (x = y) -> Void) -> (x :: xs = y :: ys) -> Void
unequalHeads contra Refl = contra Refl

unequalTails : {xs : MyVect a n} -> {ys : MyVect a n} ->
             (contra : (xs = ys) -> Void) -> (x :: xs = y :: ys) -> Void
unequalTails contra Refl = contra Refl

DecEq a => DecEq (MyVect n a) where
  decEq Nil Nil = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
    No contra => No (unequalHeads contra)
    Yes prf => case decEq xs ys of
                    No contra => No (unequalTails contra)
                    Yes prf2 => rewrite prf in Yes (cong prf2)
