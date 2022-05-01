module Queue1 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

data Queue a = ConsQ [a]
{-
    INV.REP: En ConsQ [] se cumple que:
        * Los elementos deben encolarse por delante de la lista y desencolarse del final.
-}

-- Costo: Constante
emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = ConsQ []

-- Costo: Constante
isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (ConsQ []) = True
isEmptyQ _          = False

-- Costo: Constante
queue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue elemento (ConsQ cola) = ConsQ (elemento:cola)

-- Costo: Constante
firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (ConsQ cola) = head cola

-- Costo: Lineal
dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (ConsQ cola) = ConsQ (tail cola)

obtenerUltimo :: a -> [a] -> a
obtenerUltimo e []     = e
obtenerUltimo e (x:xs) = obtenerUltimo e xs