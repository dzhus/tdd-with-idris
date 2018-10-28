module Ex6.2

import Data.Vect

-- 6.2.1
Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

-- 6.2.3
TupleVect : (n : Nat) -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)
