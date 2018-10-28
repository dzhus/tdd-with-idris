module Ex6.3

{-|

Copied from

https://github.com/edwinb/TypeDD-Samples/blob/master/Chapter6/DataStore.idr

Copyright (c) 2017 Manning Publications Co.

-}
import Data.Vect

infixr 5 .+.

data Schema = SChar| SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SChar = Char
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

data Command : Schema -> Type where
     SetSchema : Schema -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Maybe Integer -> Command schema
     Quit : Command schema


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SChar input
  = case span isAlpha (unpack input) of
         ([c], rest) => Just (c, ltrim (pack rest))
         _ => Nothing
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
        = case span (/= '"') xs of
               (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
               _ => Nothing
    getQuoted _ = Nothing

parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = do
  (l_val, input') <- parsePrefix schemal input
  (r_val, input'') <- parsePrefix schemar input'
  Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = case parsePrefix schema x of
                              Nothing => Nothing
                              Just (res, "") => Just res
                              Just _ => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: []) = Just SChar
parseSchema ("Char" :: xs) = map (SChar .+.) $ parseSchema xs
parseSchema ("String" :: []) = Just SString
parseSchema ("String" :: xs) = map (SString .+.) $ parseSchema xs
parseSchema ("Int" :: []) = Just SInt
parseSchema ("Int" :: xs) = map (SInt .+.) $ parseSchema xs
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = do
  restok <- parseBySchema schema rest
  Just (Add restok)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                    False => Nothing
                                    True => Just (Get (Just $ cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do
  schemaok <- parseSchema (words rest)
  Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SChar} item = show item
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (y .+. z)} (iteml, itemr) = display iteml ++ ", " ++
                                              display itemr

getEntry : (pos : Maybe Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry Nothing store = let entries = map display (items store) in
  Just (concat $ map (++ "\n") entries, store)
getEntry (Just pos) store = let store_items = items store in
          case integerToFin pos (size store) of
               Nothing => Just ("Out of range\n", store)
               Just id => Just (display (index id (items store)) ++ "\n", store)

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
    = case parse (schema store) input of
           Nothing => Just ("Invalid command\n", store)
           Just (Add item) =>
              Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           Just (SetSchema schema') =>
              case setSchema store schema' of
                   Nothing => Just ("Can't update schema when entries in store\n", store)
                   Just store' => Just ("OK\n", store')
           Just (Get pos) => getEntry pos store
           Just Quit => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
