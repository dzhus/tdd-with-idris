module Ex7.1

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

-- 7.1.1
Eq Shape where
  (Triangle a1 h1) == (Triangle a2 h2) = a1 == a2 && h1 == h2
  (Rectangle a1 b1) == (Rectangle a2 b2) = a1 == a2 && b1 == b2
  (Circle a) == (Circle b) = a == b
  a == b = False

area : Shape -> Double
area (Circle r) = pi * r * r
area (Triangle a h) = a * h / 2
area (Rectangle a b) = a * b

-- 7.1.2
Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
