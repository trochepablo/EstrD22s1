module PriorityQueue2 (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = PQ [a]

{-
    INV.REP: En (PQ xs) cumple que:
        * los elementos de xs estan ordenador de menor a mayor.
        Ejemplos:[4,2,3,1] No valido
                [1,2,3,4] Valido
        Comentario: es la implementación de lista ordenadas.
-}

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

-- costo: O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
insertPQ e (PQ xs) = PQ (ordenar e xs)

-- costo: O(n)
ordenar :: Ord a => a -> [a] -> [a]
-- proposito: inserta el elemento e en la posición de la lista según su orden.
ordenar elemento []     = [elemento]
ordenar elemento (x:xs) =
    if elemento < x
        then elemento : x : xs
        else x : ordenar elemento xs

-- costo: O(1)
findMinPQ :: Ord a => PriorityQueue a -> a
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = head xs

-- costo: O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = PQ (tail xs)