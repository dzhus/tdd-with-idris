module Ex5.3

import Data.Vect

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

-- 5.3.3
readVectF : (f : File) -> IO (n : Nat ** Vect n String)
readVectF f = do
  eof <- fEOF f
  if eof
  then pure (_ ** [])
  else do
    Right x <- fGetLine f | Left _ => pure (_ ** [])
    (_ ** rest) <- readVectF f
    pure (_ ** x :: rest)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right f <- openFile filename Read | Left err => pure (_ ** [])
  readVectF f
