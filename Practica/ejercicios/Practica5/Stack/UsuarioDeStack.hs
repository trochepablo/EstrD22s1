import Stack1

apilar :: [a] -> Stack a
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar stack =
    if isEmptyS stack
        then []
        else top stack : desapilar (pop stack)

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos 0 elemento stack = push elemento stack
insertarEnPos n elemento stack = push (top stack) (insertarEnPos (n-1) elemento (pop stack))