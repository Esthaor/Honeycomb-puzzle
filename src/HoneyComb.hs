{-# OPTIONS_GHC -fno-warn-tabs #-}
{-
	Title:		HoneyComb puzzle
	Authors:	Grzegorz Majchrzak, Jędrzej Blak
	Subject:	SPOP
	Project:	Zadanie projektowe z języka Haskell 
-}

module Main where
import Data.List
import Data.Char
import System.IO


-- Data types

data Field = Empty | A | B | C | D | E | F | G deriving (Show, Read, Eq) 
data Comb = Comb { rows :: [[Field]] } deriving (Show, Read)
data Plaster = Plaster [String] deriving (Show, Read)

avaliableFields = [A,B,C,D,E,F,G]

-- Data converters and accesors

strToPlaster :: String -> Plaster
strToPlaster combStr = read combStr

charToField :: String -> Field
charToField "." = Empty
charToField x = read x

strToFieldTable :: String -> [Field]
strToFieldTable [] = []
strToFieldTable (x:xs) = charToField([x]) : strToFieldTable(xs)

combStrToTables :: [String] -> [[Field]]
combStrToTables [] = []
combStrToTables (x:xs) = strToFieldTable(x) : combStrToTables(xs)

plasterToComb :: Plaster -> Comb
plasterToComb (Plaster a) = Comb (combStrToTables a)

-- row - get row with x index
row :: Comb -> Int -> [Field]
row c x = (rows c) !! x

-- field - get field from x row and y column
field :: Comb -> Int -> Int -> Field
field c i j = ((rows c) !! i) !! j

-- Main functions

removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
 | otherwise = y : removeItem x ys

next :: Field -> Field
next Empty = Empty
next A = B
next B = C
next C = D
next D = E
next E = F
next F = G
next G = Empty

-- Checks if puzzle has every field different than Empty
isSolved :: Comb -> Bool
isSolved c = and [Empty `notElem` row | row <- (rows c)]

-- Checks if all connected fields have different values in whole Comb
isCombOk :: Comb -> Bool
isCombOk x = and [isRowOk x i | i <- [0..(length(rows x)-1)]]

-- Check if all connected fields have different values in row with i index
isRowOk :: Comb -> Int -> Bool
isRowOk x i= and [isFieldOk x i j | j <- [0..(length(row x i)-1)]]

-- Check if all connected fields have different values in field (i, j)
isFieldOk :: Comb -> Int-> Int -> Bool
isFieldOk x i j | (i == 0 && j == 0) 
  = isOkBlock [field x i j, field x i (j+1), field x (i+1) j, field x (i+1) (j+1)]
 | (i == 0 && j == ((length (row x i))-1)) 
  = isOkBlock [field x i j, field x i (j-1), field x (i+1) j, field x (i+1) (j+1)]
 | (i == ((length (rows x))-1) && j == 0) 
  = isOkBlock [field x i j, field x i (j+1), field x (i-1) j, field x (i-1) (j+1)]
 | (i == ((length (rows x))-1) && j == ((length (row x i))-1)) 
  = isOkBlock [field x i j, field x i (j-1), field x (i-1) j, field x (i-1) (j+1)]
 | (i == 0)
  = isOkBlock [field x i j, field x i (j-1), field x i (j+1), field x (i+1) j, field x (i+1) (j+1)]
 | (i == ((length (rows x))-1)) 
  = isOkBlock [field x i j, field x i (j-1), field x i (j+1), field x (i-1) j, field x (i-1) (j-1)]
 | (i `mod` 2 == 1 && j == 0) 
  = isOkBlock [field x i j, field x i (j+1), field x (i-1) j, field x (i+1) j]
 | (i `mod` 2 == 1 && j == (length (row x i)-1)) 
  = isOkBlock [field x i j, field x i (j-1), field x (i-1) (j-1), field x (i+1) (j-1)]
 | (i `mod` 2 == 0 && j == 0)
  = isOkBlock [field x i j, field x i (j+1), field x (i-1) j, field x (i-1) (j+1), field x (i+1) j, field x (i+1) (j+1)]
 | (i `mod` 2 == 0 && j == (length (row x i)-1)) 
  = isOkBlock [field x i j, field x i (j-1), field x (i-1) j, field x (i-1) (j+1), field x (i+1) j, field x (i+1) (j+1)]
 | (i `mod` 2 == 1)
  = isOkBlock [field x i j, field x i (j-1), field x i (j+1), field x (i-1) (j-1), field x (i-1) j, field x (i+1) (j-1), field x (i+1) j]
 | otherwise
  = isOkBlock [field x i j, field x i (j-1), field x i (j+1), field x (i-1) j, field x (i-1) (j+1), field x (i+1) j, field x (i+1) (j+1)]
 
-- Check if all Fields in list have different values or Empty
isOkBlock :: [Field] -> Bool
isOkBlock [] = True
isOkBlock (Empty:xs) = isOkBlock xs
isOkBlock (x:xs) = (notElem x xs) && (isOkBlock xs)
  
-- test = do
-- list <- [removeItem C avaliableFields]
-- list <- [removeItem B list]
-- show list

--main - reads file and starts main algorithm
main = do 
		putStrLn "File name:"
		name <- getLine
		handle <- openFile ("../files/" ++ name) ReadMode
		combStr <- hGetContents handle
		if (isCombOk (plasterToComb (strToPlaster combStr)))
			then putStrLn "TAK"
			else putStrLn "NIE"