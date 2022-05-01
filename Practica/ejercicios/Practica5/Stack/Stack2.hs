module Stack2 (Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = [a]
{-
    INV.REP: En [] se debe cumplir:
        * Los ultimos elementos agregados son los primeros en salir
-}

emptyS :: Stack a
emptyS = []

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS [] = True
isEmptyS _  = False

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push a lista = a:lista

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top lista = head stack

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
pop (x:xs) = xs

lenS :: Stack a -> Int
lenS []     = 0
lens (x:xs) = 1 + lenS xs
