module Set1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

--Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.

data Set a = ConS [a]
          --   elementos   Cant. elementos
{- INV.REP.: en (Set elementos Int), se cumple que
    * no hay elementos repetidos
    * en cant no hay numeros negativos
-}

-- costo: constante
emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = ConS []

-- costo: 
addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS e conjunto = 
    if belongs e conjunto
        then conjunto
        else agregarA e conjunto

agregarA :: Eq a => a -> Set a -> Set a
agregarA e (ConS els) = ConS (e:els)

-- costo: linea
belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs e (ConS els) = perteneceA e els


perteneceA :: Eq a => a -> [a] -> Bool
perteneceA e []     = False
perteneceA e (x:xs) = e == x || perteneceA e xs

-- costo: lineal
sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (ConS els) = length els


-- Costo: lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConS xs) = ConS (sacarUno x xs)

-- Costo: lineal
sacarUno :: Eq a => a -> [a] -> [a]
sacarUno x [] = []
sacarUno x (y:ys) =
	if x == y
	   then ys
	   else y : sacarUno x ys


-- Costo: cuadratica
unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (ConS xs) (ConS ys) = ConS (unirListas xs ys)

-- Costo: cuadratica
unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] ys = ys
unirListas (x:xs) ys =
	if elem x ys
	   then unirListas xs ys
	   else x : unirListas xs ys

-- Costo: constante
setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (ConS els) = els