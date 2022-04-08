module Practica2 where
import Practica1


-- 1) --

--1. 
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

--2. 
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = sucesor x : sucesores xs

--4. 
conjuncion :: [Bool] -> Bool
conjuncion []     = True 
conjuncion (x:xs) = x && conjuncion xs

--5 
disyuncion :: [Bool] -> Bool
disyuncion []     = False 
disyuncion (x:xs) = x || disyuncion xs

-- 6 
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = e == x || pertenece e xs 

--8
apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = 
    if e == x
        then 1 + apariciones e xs
        else apariciones e xs

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = 
    if n < x
        then x : losMenoresA n xs
        else losMenoresA n xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = 
    if longitud x > n
        then [x] ++ lasDeLongitudMayorA n xs
        else lasDeLongitudMayorA n xs

--11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e] 
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

--12 
concatenar :: [a] -> [a] -> [a]
concatenar [] ys     = ys
concatenar (x:xs) ys = x : concatenar xs ys

--13
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = concatenar (reversa xs) [x]

-- reversa :: [a] -> [a]
-- reversa []     = []
-- reversa (x:xs) = reversa xs ++ [x]

--14 
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _          = []
zipMaximos _ []          = []
zipMaximos (x:xs) (y:ys) = 
    if x > y
        then x : zipMaximos xs ys
        else y : zipMaximos xs ys

--15 
elMinimo :: Ord a => [a] -> a
elMinimo []     = error "Debe haber al menos 1 elemento"
elMinimo (x:[]) = x
elMinimo (x:xs) = 
    if x < elMinimo xs
        then x
        else elMinimo xs

