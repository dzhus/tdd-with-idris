module Ex5.1

-- 5.1.1
printLonger : IO ()
printLonger = do
  l1 <- getLine
  l2 <- getLine
  putStrLn $ show $ max (length l1) (length l2)

-- 5.1.2
printLonger_do : IO ()
printLonger_do = getLine >>= \l1 => getLine >>= \l2 =>
  putStrLn $ show $ max (length l1) (length l2)
