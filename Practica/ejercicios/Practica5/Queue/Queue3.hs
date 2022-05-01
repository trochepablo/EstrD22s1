module Queue3 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

data Queue a = ConsQ [a] [a] deriving Show
{-
    INV.REP: En ConsQ [] se cumple que:
        * Los elementos deben encolarse al final de la lista y desencolarse por delante
-}

-- Costo: Constante
emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = ConsQ [] []

-- Costo: Constante
isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (ConsQ [] bs) = True
isEmptyQ _             = False

-- Costo: Constante
queue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue elemento (ConsQ [] bs) = ConsQ [elemento] bs
queue elemento (ConsQ fs bs) = ConsQ fs (elemento:bs)

-- Costo: Constante
firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (ConsQ fs bs) = head fs

-- Costo: Linal
dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (ConsQ (f:[]) bs) = ConsQ (ordQueue bs) []
dequeue (ConsQ fs bs)     = ConsQ (tail fs) bs

ordQueue :: [a] -> [a]
ordQueue lista = reverse lista