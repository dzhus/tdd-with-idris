module Main

main : IO ()
main = putStrLn "Hello world"

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

data MyNat = Z | S MyNat

plus : MyNat -> MyNat -> MyNat
plus Z     y = y
plus (S k) y = plus k (S y)

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd Z = False
  odd (S k) = even k

data Fin : Nat -> Type where
   FZ : Fin (S k)
   FS : Fin k -> Fin (S k)

-- data Vect : Nat -> Type -> Type where
--    Nil  : Vect Z a
--    (::) : a -> Vect k a -> Vect (S k) a

-- isEmpty : Vect n a -> Bool
-- isEmpty {n = Z} _   = True
-- isEmpty {n = S k} _ = False

-- e : Vect Z Int
-- e = Nil

-- data File -- abstract
-- data Mode = Read | Write | ReadWrite

-- kek : String -> Main.Mode -> String
-- kek _ Read = "read"

data Main.Stream : Type -> Type where
  (::) : (e : a) -> Inf (Main.Stream a) -> Main.Stream a

ones : Main.Stream Nat
ones = 1 :: ones
