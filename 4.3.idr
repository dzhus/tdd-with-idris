module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs

-- 4.3.1, 4.3.2
data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" arg = Just $ Add arg
parseCommand "get" sId = if all isDigit (unpack sId)
                         then (Just $ Get $ cast sId)
                         else Nothing
parseCommand "search" needle = Just $ Search needle
parseCommand "size" _ = Just Size
parseCommand "quit" _ = Just Quit
parseCommand _ _ = Nothing

findMatches : String -> DataStore -> (p : Nat ** Vect p String)
findMatches needle ds = filter (isInfixOf needle) $ items ds

processCommand_search : (needle : String) -> (ds : DataStore) -> Maybe (String, DataStore)
processCommand_search needle ds =
  case findMatches needle ds of
       (Z ** _) => Just ("No results\n", ds)
       (r ** ms) => Just (concat $ map (++ "\n") ms, ds)

processCommand : (cmd : Command) -> (ds : DataStore) -> Maybe (String, DataStore)
processCommand (Add s) ds = Just ("Added " ++ s ++ "\n", addToStore ds s)
processCommand (Get i) ds = case integerToFin i (size ds) of
                                 Just i' => Just (index i' (items ds) ++ "\n", ds)
                                 Nothing => Just ("Wrong index\n", ds)
processCommand (Search needle) ds = processCommand_search needle ds
processCommand Size ds = Just (cast (size ds) ++ "\n", ds)
processCommand Quit _ = Nothing

-- Common
parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd $ ltrim args

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds inp =
  case parse inp of
    Nothing => Just ("Invalid command\n", ds)
    Just cmd => processCommand cmd ds

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
