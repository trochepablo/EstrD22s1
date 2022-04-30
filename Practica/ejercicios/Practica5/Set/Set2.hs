module Set1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

--Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.

data Set a = ConsS [a]
-- Invariantes de Representacion
-- Sea ConsS xs un conjunto
-- 1. xs no tiene elementos repetidos

-- Costo: constante
emptyS :: Set a
emptyS = ConsS []

-- Costo: constante
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) = ConsS (x:xs)

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

-- Costo: cuadratico
sizeS :: Set a -> Int
sizeS (ConsS xs) = length (sinRepetidos xs)

-- Costo: cuadratico
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarUno x (sinRepetidos xs))

sacarUno :: Eq a => a -> [a] -> [a]
sacarUno x [] = []
sacarUno x (y:ys) =
	if x == y
	   then ys
	   else y : sacarUno x ys

-- Costo: cuadratica
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (unirListas xs ys)

-- Costo: cuadratica
unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] ys = ys
unirListas (x:xs) ys = x : append xs ys

-- Costo: lineal
append :: Eq a => [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Sin repetidos
-- Costo: constante
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = sinRepetidos xs

sinRepetidos :: Eq => [a] -> [a]
sinRepetidos []     =
sinRepetidos (x:xs) =
    if elem x xs
        then sinRepetidos xs
        else x : sinRepetidos xs