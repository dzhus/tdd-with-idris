module Ex7.3

-- 7.3.1
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Functor Expr where
  map func (Val x)   = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x)   = Abs (map func x)

-- 7.3.2
data Vect : Nat -> el -> Type where
  Nil : Vect 0 el
  (::) : el -> Vect k el -> Vect (S k) el

Eq e => Eq (Vect n e) where
  (==) (v1 :: v1s) (v2 :: v2s) = v1 == v2 && v1s == v2s
  (==) [] [] = True

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (v :: vs) = func v (foldr func init vs)
  foldl func init [] = init
  foldl func init (v :: vs) = foldl func (func init v) vs
