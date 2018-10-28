module Ex5.3

-- 5.3.1
readToBlank : IO (List String)
readToBlank = do
  l <- getLine
  case l of
    "" => pure []
    s  => readToBlank >>= \rest => pure (s :: rest)
