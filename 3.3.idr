module Matrix

import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix k j x = Vect k (Vect j x)

transposeM : Matrix m n e -> Matrix n m e
transposeM (x :: xs) = let xsTrans = transposeM xs in (zipWith (::) x xsTrans)
transposeM [] = replicate _ []

addMatrix_rhs_1 : Num e => (x : Vect n e) -> (y : Vect n e) -> Vect n e
addMatrix_rhs_1 x y = zipWith (+) x y

addMatrix : Num e => Matrix m n e -> Matrix m n e -> Matrix m n e
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addMatrix_rhs_1 x y :: addMatrix xs ys

multHelper : Num e => (x : Vect n e) -> (rightT : Matrix p n e) -> Vect p e
multHelper _ [] = []
multHelper row (y :: ys) = let newEl = foldl (+) 0 (zipWith (*) row y) in
                           newEl :: (multHelper row ys)

multMatrix : Num e => Matrix m n e -> Matrix n p e -> Matrix m p e
multMatrix [] [] = []
multMatrix [] (x :: xs) = []
multMatrix (x :: xs) right = let rightT = transposeM right in multHelper x rightT :: (multMatrix xs right)
