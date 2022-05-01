import Queue1

lengthQ :: Queue a -> Int
-- Cuenta la cantidad de elementos de la cola.
lengthQ cola = 
    if isEmptyQ cola
        then 0
        else 1 + lengthQ (dequeue cola)

queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos,
-- donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList cola =
    if isEmptyQ cola
        then []
        else firstQ cola : queueToList (dequeue cola)

unionQ :: Queue a -> Queue a -> Queue a
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ cola1 cola2 =
    if isEmptyQ cola1
        then cola2
        else queue (firstQ cola1) (unionQ (dequeue cola1) cola2)

-- queueToList (queue 7 (queue 4 (queue 1 emptyQ)))         