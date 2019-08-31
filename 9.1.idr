module Ex9.1

-- 9.1.1

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

-- 9.1.2

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

total notInEmpty : Last [] value -> Void
notInEmpty LastOne impossible
notInEmpty (LastCons _) impossible

total notIsOnly : (contra : (x = value) -> Void) -> Last [x] value -> Void
notIsOnly contra LastOne = contra Refl
notIsOnly contra (LastCons prf) impossible

total notInTail : (Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notInTail contra (LastCons prf) = contra prf

total isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInEmpty
isLast (x :: []) value =
  case decEq x value of
    Yes Refl => Yes LastOne
    No contra => No (notIsOnly contra)
isLast (x :: (y :: xs)) value =
  case isLast (y :: xs) value of
    (Yes prf) => Yes (LastCons prf)
    (No contra) => No (notInTail contra)
