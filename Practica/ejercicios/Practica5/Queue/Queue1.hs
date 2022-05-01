module Queue1 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

data Queue a = ConsQ [a] deriving Show
{-
    INV.REP: En ConsQ [] se cumple que:
        * Los elementos deben encolarse al final de la lista y desencolarse por delante
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

-- Costo: Lineal
queue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue elemento (ConsQ cola) = ConsQ (agregarAlFinal elemento cola)

agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal e []     = [e]
agregarAlFinal e (x:xs) = x : agregarAlFinal e xs

-- Costo: Constante
firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (ConsQ cola) = head cola

-- Costo: Constante
dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (ConsQ cola) = ConsQ (tail cola)