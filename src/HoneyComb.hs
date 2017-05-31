{-
	Title:		HoneyComb puzzle
	Authors:	Grzegorz Majchrzak, Jędrzej Blak
	Subject:	SPOP
	Project:	Zadanie projektowe z języka Haskell 
-}

module Main where
import Data.Maybe
import Data.List
import Data.Char
import System.IO
import System.IO.Error


-- Data types

data Field = Empty | A | B | C | D | E | F | G deriving (Show, Read, Eq) 
data Comb = Comb { rows :: [[Field]] } deriving (Show, Read)
data Plaster = Plaster [String] deriving (Show, Read)
data Coords = Coords {rowVal::Int, columnVal::Int} deriving (Show, Read)

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

-- return new list with replaced nth element
replaceNth n newVal (x:xs) | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs 

-- return new Comb with replaced (i,j) element as f 
placeFieldIntoComb :: Comb -> Int -> Int -> Field -> Comb
placeFieldIntoComb c i j f = Comb (replaceNth i (replaceNth j f (row c i)) (rows c))

-- return all neighbor values and itself
getFieldNeighborhood :: Comb -> Int -> Int -> [Field]
getFieldNeighborhood c i j | (i == 0 && j == 0) 
  = [field c i j, field c i (j+1), field c (i+1) j, field c (i+1) (j+1)]
 | (i == 0 && j == ((length (row c i))-1)) 
  = [field c i j, field c i (j-1), field c (i+1) j, field c (i+1) (j+1)]
 | (i == ((length (rows c))-1) && j == 0) 
  = [field c i j, field c i (j+1), field c (i-1) j, field c (i-1) (j+1)]
 | (i == ((length (rows c))-1) && j == ((length (row c i))-1)) 
  = [field c i j, field c i (j-1), field c (i-1) j, field c (i-1) (j+1)]
 | (i == 0)
  = [field c i j, field c i (j-1), field c i (j+1), field c (i+1) j, field c (i+1) (j+1)]
 | (i == ((length (rows c))-1)) 
  = [field c i j, field c i (j-1), field c i (j+1), field c (i-1) j, field c (i-1) (j+1)]
 | (i `mod` 2 == 1 && j == 0) 
  = [field c i j, field c i (j+1), field c (i-1) j, field c (i+1) j]
 | (i `mod` 2 == 1 && j == (length (row c i)-1)) 
  = [field c i j, field c i (j-1), field c (i-1) (j-1), field c (i+1) (j-1)]
 | (i `mod` 2 == 0 && j == 0)
  = [field c i j, field c i (j+1), field c (i-1) j, field c (i-1) (j+1), field c (i+1) j, field c (i+1) (j+1)]
 | (i `mod` 2 == 0 && j == (length (row c i)-1)) 
  = [field c i j, field c i (j-1), field c (i-1) j, field c (i-1) (j+1), field c (i+1) j, field c (i+1) (j+1)]
 | (i `mod` 2 == 1)
  = [field c i j, field c i (j-1), field c i (j+1), field c (i-1) (j-1), field c (i-1) j, field c (i+1) (j-1), field c (i+1) j]
 | otherwise
  = [field c i j, field c i (j-1), field c i (j+1), field c (i-1) j, field c (i-1) (j+1), field c (i+1) j, field c (i+1) (j+1)]

-- get first Empty from row i
getFirstEmpty :: Comb -> Int -> Coords
getFirstEmpty c i | i == length (rows c) = Coords (-1) (-1)
 | (elemIndex Empty (row c i)) == Nothing = getFirstEmpty c (i+1)
 | otherwise = Coords i (fromJust (elemIndex Empty (row c i)))

{-
	Make func return best Coords (Empty field with almost all neighbor)
	getBestInputField :: Comb -> Coords
-}

-- Write to file

writeCombToFile :: Comb -> String -> IO Bool
writeCombToFile c name = do
 writeFile ("../files/" ++ name ++ ".ans") ((show c) ++ "\n")
 putStrLn ("Wrote to file " ++ name ++ ".ans\n" ++ (show c)) 
 return True
 
-- Validators

-- Check if every row's length is correct 
isCombValid :: Comb -> Bool
isCombValid x = length(rows x) `mod` 2 == 1 && and [isRowValid x (length(rows x)) i | i <- [0..(length(rows x)-1)]]

-- Check if row length is correct according to (n-1), n, (n-1), ..., (n-1) rule
isRowValid :: Comb -> Int -> Int -> Bool
isRowValid x n i | (i `mod` 2 == 0)
  = length(row x i) == n-1
 | (i `mod` 2 == 1)
  = length(row x i) == n
 
-- Main functions

-- Removes item from list
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
 | otherwise = y : removeItem x ys

-- Remove all items form xs from list ys
removeAllItem _ [] = []
removeAllItem xs (y:ys) | y `elem` xs = removeAllItem xs ys
 | otherwise = y : removeAllItem xs ys

-- Next value of Field
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
isSolved c = (and [Empty `notElem` row | row <- (rows c)]) && isCombOk c

-- Checks if all connected fields have different values in whole Comb
isCombOk :: Comb -> Bool
isCombOk x = and [isRowOk x i | i <- [0..(length(rows x)-1)]]

-- Check if all connected fields have different values in row with i index
isRowOk :: Comb -> Int -> Bool
isRowOk x i= and [isFieldOk x i j | j <- [0..(length(row x i)-1)]]

-- Check if all connected fields have different values in field (i, j)
isFieldOk :: Comb -> Int-> Int -> Bool
isFieldOk c i j = isBlockOk (getFieldNeighborhood c i j)

-- Check if all Fields in list have different values or Empty
isBlockOk :: [Field] -> Bool
isBlockOk [] = True
isBlockOk (Empty:xs) = isBlockOk xs
isBlockOk (x:xs) = (notElem x xs) && (isBlockOk xs)

-- Possible fields in i row and j column Coords
possibleFields :: Comb -> Int -> Int -> [Field]
possibleFields c i j = removeAllItem (getFieldNeighborhood c i j) avaliableFields

solveComb :: Comb -> String -> Int -> Int -> [Field] -> IO Bool
solveComb c name i j [] = return False
solveComb c name i j (x:xs) = do
 let comb = placeFieldIntoComb c i j x
 putStrLn(show comb)
 if (isCombOk comb)
  then do
   if (isSolved comb)
    then writeCombToFile comb name
    else do
     let coords = (getFirstEmpty comb 0)
     let new_i = rowVal coords
     let new_j = columnVal coords
     let fields = possibleFields comb new_i new_j
     wasOk <- solveComb comb name new_i new_j fields
     if (not wasOk) 
      then solveComb comb name i j xs
      else return True
  else 
   solveComb comb name i j xs

--main - reads file and starts main algorithm
main = do 
 putStrLn "File name:"
 name <- getLine
 handle <- openFile ("../files/" ++ name) ReadMode
 combStr <- hGetContents handle
 let comb = plasterToComb (strToPlaster combStr)
 if(isCombValid comb)
  then do
  let coords = (getFirstEmpty comb 0)
  let i = rowVal coords
  let j = columnVal coords
  if (i == -1) 
   then
    writeCombToFile comb name
   else do
    let fields = possibleFields comb i j
    let f = fields !! 0
    solveComb (placeFieldIntoComb comb i j f) name i j (removeItem f fields)
  else do
      putStrLn "Invalid input data!"
      return False
