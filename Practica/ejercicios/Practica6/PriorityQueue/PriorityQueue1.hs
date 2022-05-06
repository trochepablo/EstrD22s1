module PriorityQueue1 (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

-- costo: O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
insertPQ e (PQ xs) = PQ e:xs

-- costo: O(n)
findMinPQ :: Ord a => PriorityQueue a -> a
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = minimum xs

-- costo: O(n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = PQ (eliminarMinimo (minimum xs) xs)

-- costo: O(n)
eliminarMinimo :: Eq a => a -> [a] -> [a]
-- Propósito: elimina al elemento de la lista
eliminarMinimo elemento []     = []
eliminarMinimo elemento (x:xs) = if elemento == x then xs else x : eliminarMinimo elemento xs