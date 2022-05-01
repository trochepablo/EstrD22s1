module Stack1 (Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = ConsST [a]
{-
    INV.REP: En ConsST [] se debe cumplir:
        * Los ultimos elementos agregados son los primeros en salir
-}

emptyS :: Stack a
-- Crea una pila vacía.
emptyS = ConsST []

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS (ConsST []) = True
isEmptyS _           = False

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push a (ConsST stack) = ConsST (a:stack)

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top (ConsST stack) = head stack

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
pop (ConsST (x:xs)) = ConsST xs

lenS :: Stack a -> Int
lenS (ConsST stack) = contarElementosDe stack

contarElementosDe :: [a] -> Int
contarElementosDe [] = 0
contarElementosDe (x:xs) = 1 + contarElementosDe xs
