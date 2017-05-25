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

data Field = Empty | A | B | C | D | E | F | G deriving Show 
data Plaster = Plaster [String] deriving (Show, Read)

next :: Field -> Field
next Empty = A
next A = B
next B = C
next C = D
next D = E
next E = F
next F = G
next G = Empty

strToPlaster :: String -> Plaster
strToPlaster combStr = read combStr

isCombOk :: Plaster -> Bool
isCombOk x = True

main = do
 putStrLn "File name:"
 name <- getLine
 handle <- openFile ("../files/" ++ name) ReadMode
 combStr <- hGetContents handle
 if (isCombOk (strToPlaster combStr))
  then putStrLn "TAK"
  else putStrLn "NIE"