module Ex8.1

-- 8.1.1
total same_cons : {xs : List a} -> {ys: List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

-- 8.1.2
total same_lists : {xs : List a } -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl tails = cong tails

-- 8.1.3
data ThreeEq : ( a : Nat ) -> ( b : Nat ) -> ( c : Nat ) -> Type where
  Refl3 : ( a : Nat ) -> ThreeEq a a a

-- 8.1.4
total allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Refl3 z) = Refl3 (S z)
