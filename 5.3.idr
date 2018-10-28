module Ex5.3

-- 5.3.1
readToBlank : IO (List String)
readToBlank = do
  l <- getLine
  case l of
    "" => pure []
    s  => readToBlank >>= \rest => pure (s :: rest)

-- 5.3.2
readAndSave : IO ()
readAndSave = do
  input <- readToBlank
  fName <- getLine
  Right () <- writeFile fName (concat $ map (++ "\n") input)
  | Left err => putStrLn (show err)
  pure ()
