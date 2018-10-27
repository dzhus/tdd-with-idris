module Ex4.2

import Data.Vect

-- 4.2.1, 4.2.2
data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : Vehicle Electric

total wheels : Vehicle power -> Nat
wheels Tram = 4
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

total refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Bus fuel) = Bus 200

-- 4.2.3, 4.2.4
total vectTake : (m : Fin (S n)) -> Vect n a -> Vect (finToNat m) a
vectTake FZ xs = []
vectTake (FS i) (x :: xs) = x :: vectTake i xs

-- 4.2.5
total sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
  Nothing => Nothing
  Just x => Just $ index x xs + index x ys
