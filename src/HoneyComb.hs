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
data Comb a = Comb [[Field]] deriving (Show, Read)
data Plaster a = Plaster [String] deriving (Show, Read)
avaliableFields = [A,B,C,D,E,F,G]


-- Data converters

strToPlaster :: String -> Plaster a
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

plasterToComb :: Plaster a -> Comb a
plasterToComb (Plaster a) = Comb (combStrToTables a)

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

isCombOk :: Comb a -> Bool
isCombOk x = True

--test = do
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