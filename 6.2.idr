module Ex6.2

import Data.Vect

-- 6.2.1
Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)
