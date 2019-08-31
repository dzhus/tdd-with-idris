module Ex10.1

-- 10.1.1

data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) =
  case takeN k xs of
    Fewer => Fewer
    Exact n_xs => Exact (x :: n_xs)

-- 10.1.2

halves : List a -> (List a, List a)
halves xs with (takeN (div (length xs) 2) xs)
  halves xs             | Fewer        = ([], xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)
