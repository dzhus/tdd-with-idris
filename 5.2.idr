module Ex5.2

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if (input /= "") && all isDigit (unpack input)
  then pure (Just $ cast input)
  else pure Nothing

-- 5.2.1
guess : (target : Nat) -> IO ()
guess t = do
  inp <- readNumber
  case inp of
    Nothing => do
      putStrLn $ "Please enter a number.\n"
      guess t
    Just n => do
      case compare n t of
        LT => do
          putStrLn "Too small.\n"
          guess t
        EQ => putStrLn "Correct!\n"
        GT => do
          putStrLn "Too large.\n"
          guess t

-- 5.2.2
main : IO ()
main = do
  time >>= \seed => guess (1 + (cast seed) `mod` 100)

-- 5.2.3
guess_v2 : (target : Nat) -> (guesses : Nat) -> IO ()
guess_v2 t guesses = do
  putStrLn $ show guesses ++ " guesses."
  inp <- readNumber
  case inp of
    Nothing => do
      putStrLn $ "Please enter a number.\n"
      guess_v2 t guesses
    Just n => do
      case compare n t of
        LT => do
          putStrLn "Too small.\n"
          guess_v2 t (S guesses)
        EQ => putStrLn "Correct!\n"
        GT => do
          putStrLn "Too large.\n"
          guess_v2 t (S guesses)

main_v2 : IO ()
main_v2 = do
  time >>= \seed => guess_v2 (1 + (cast seed) `mod` 100) 0

-- 5.2.4
my_repl : String -> (String -> String) -> IO ()
my_repl prompt proc = do
  putStrLn prompt
  getLine >>= \input => putStrLn (proc input)
  my_repl prompt proc

my_replWith : s -> String -> (s -> String -> Maybe (String, s)) -> IO ()
my_replWith state prompt proc = do
  putStrLn prompt
  getLine >>= \input =>
    case proc state input of
      Nothing => pure ()
      Just (output, newState) => do
        putStrLn output
        my_replWith newState prompt proc
