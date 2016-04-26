import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char
import Data.String

petal0 =[0,0,0,0,0,0,0]
petal1 =[0,0,0,0,0,0,0]
petal2 =[0,0,0,0,0,0,0]
petal3 =[0,0,0,0,0,0,0]
petal4 =[0,0,0,0,0,0,0]
petal5 =[0,0,0,0,0,0,0]
petal6 =[0,0,0,0,0,0,0]

cwSpiral0 = [petal0!!0, petal1!!0, petal2!!0, petal3!!0, petal4!!0, petal5!!0, petal6!!0]
cwSpiral1 = [petal0!!1, petal1!!1, petal2!!1, petal3!!1, petal4!!1, petal5!!1, petal6!!1]
cwSpiral2 = [petal0!!2, petal1!!2, petal2!!2, petal3!!2, petal4!!2, petal5!!2, petal6!!2]
cwSpiral3 = [petal0!!3, petal1!!3, petal2!!3, petal3!!3, petal4!!3, petal5!!3, petal6!!3]
cwSpiral4 = [petal0!!4, petal1!!4, petal2!!4, petal3!!4, petal4!!4, petal5!!4, petal6!!4]
cwSpiral5 = [petal0!!5, petal1!!5, petal2!!5, petal3!!5, petal4!!5, petal5!!5, petal6!!5]
cwSpiral6 = [petal0!!6, petal1!!6, petal2!!6, petal3!!6, petal4!!6, petal5!!6, petal6!!6]

--bigList = [petal0,petal1,petal2,petal3,petal4,petal5,petal6]

--THESE ARE THE ADDTO FUNCTIONS
putinpetal :: Int-> Int -> [Int] -> [Int]
putinpetal index value petal = (take x petal) ++ [value] ++ (drop (x+1) petal)
    where x = index `mod` 7

putInbigList :: Int -> Int ->[[Int]]-> [[Int]]
putInbigList index value list = take (x) list ++ [putinpetal y value (getpetal y list)] ++ drop (x+1) list
    where x = index `div` 7
	  y = index `mod` 7
--________________________________	  
--GETTING THE POTENTIAL MOVES
getPotMoves :: Int -> [[Int]] -> [Int]
getPotMoves index list = union vert a
	where vert = getcwMoves index list
   	      a = union (getRowMoves index list) (getccwMoves index list)
--________________________________

--ROW OF PETALS CHECKING

getRowMoves :: Int ->  [[Int]] -> [Int]
getRowMoves value bigList = a \\ b
    where a = [0..7]
	  b = getpetal value bigList
--_____________________________

--CLOCKWISE SPIRAL CHECKING

makecwSpiral :: Int -> [[Int]] -> [Int]
makecwSpiral index list
    |a == 0 = [getNode x list | x <-[0,7..42] ]
    |a == 1 = [getNode x list | x <-[1,8..43] ]
    |a == 2 = [getNode x list | x <-[2,9..44] ]
    |a == 3 = [getNode x list | x <-[3,10..45] ]
    |a == 4 = [getNode x list | x <-[4,11..46] ]
    |a == 5 = [getNode x list | x <-[5,12..47] ]
    |a == 6 = [getNode x list | x <-[6,13..48] ]
	where a = index `mod` 7

getcwMoves :: Int -> [[Int]] -> [Int]
getcwMoves value bigList = a\\b
	where a = [0..7]
	      b = makecwSpiral value bigList
--________________________________		 
--COUNTER-CLOCKWISE SPIRAL CHECKING

makeccwSpiral :: Int -> [[Int]] -> [Int]
makeccwSpiral index list
    |a == 0 = [getNode x list | x <-[0,13,19,25,31,37,43] ]
    |a == 1 = [getNode x list | x <-[1,7,20,26,32,38,44] ]
    |a == 2 = [getNode x list | x <-[2,8,14,27,33,39,45] ]
    |a == 3 = [getNode x list | x <-[3,9,15,21,34,40,46] ]
    |a == 4 = [getNode x list | x <-[4,10,16,22,28,41,47] ]
    |a == 5 = [getNode x list | x <-[5,11,17,23,29,35,48] ]
    |a == 6 = [getNode x list | x <-[6,12,18,24,30,36,42] ]
	where a = index `mod` 7

getccwMoves :: Int -> [[Int]] -> [Int]
getccwMoves value bigList = a\\b
	where a = [0..7]
	      b = makeccwSpiral value bigList
--___________________________________

--VARIOUS HELPER FUNCTIONS
getpetal :: Int -> [[Int]] -> [Int]
getpetal a list
    |a < 7 = list!! a

getNode :: Int -> [[Int]] -> Int
getNode index list
    |index <= 48 = list!!a!!b
    |otherwise = -1
	where a = index `div` 7
	      b = index `mod` 7
--__________________________________
--CREATING THE BOARD FUNCTIONS
createpetal :: Int -> [Int]
createpetal a = replicate a 0

addPetal :: Int -> [[Int]]
addPetal a
    |a > 1 = [createpetal 7] ++ addPetal (a-1)
    |otherwise = [createpetal 7]


--___________________________________
--EXPRESSIONS FOR TESTING

-- VERTICAL/CW

--HORIZONTAL/ROW
list8  = addPetal 7
list9  = putInbigList 0 1 list8
list10 = putInbigList 1 2 list9
list11 = putInbigList 2 3 list10
list12 = putInbigList 3 4 list11
list13 = putInbigList 4 5 list12
list14 = putInbigList 5 6 list13
list15 = putInbigList 6 7 list14

--LEFT DIAG/CCW
list16 = addPetal 7

p1=createpetal 7



main = mapM_ print list15
--CHECKS IF PLACEMENT IS VALID


