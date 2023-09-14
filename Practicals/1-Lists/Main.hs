module Main where

import Data.Char
import Data.List
import Data.Maybe
import Text.Printf (printf)
import Text.Read (Lexeme(String))

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1
parseTable :: [String] -> Table
parseTable = map words;

-- | Printing

-- * Exercise 2
printLine :: [Int] -> String
printLine list = '+' : createInbetweens list ++ "+" where
  createInbetweens ints = intercalate "+" (createMinusList ints) 
  createMinusList = map f
  f x = replicate x '-'

-- * Exercise 3
printField :: Int -> String -> String
printField l field =
  let wsNeeded = l - length field in
    if all isDigit field 
      then replicate wsNeeded ' ' ++ field 
      else field ++ replicate wsNeeded ' ' 
    


-- * Exercise 4
printRow :: [(Int, String)] -> String
printRow list = '|' : intercalate "|" (map (uncurry printField) list) ++ "|";  

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths table = 
  let goodTable = transpose table in
      map getMaximum goodTable where 
        getMaximum :: [Field] -> Int
        getMaximum column = maximum (map length column)
    

-- [["Header 1", "Header 2"], ["value 1", "another value 2"]]
-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows) =
  printHeader ++ produceContent rows  ++ [getLine] where
        
  printHeader :: [String]
  printHeader = 
    [getLine, printRow (zip (columnWidths table) (listToUpper header)), getLine];

  listToUpper :: [String] -> [String]
  listToUpper = map (map toUpper);

  getLine :: String
  getLine = printLine (columnWidths table)
  
  produceContent :: [Row] -> [String]
  produceContent content = map (getRowContent) content
        
  getRowContent :: Row -> String
  getRowContent row = printRow (zip (columnWidths table) row)
        

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = table

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = table
