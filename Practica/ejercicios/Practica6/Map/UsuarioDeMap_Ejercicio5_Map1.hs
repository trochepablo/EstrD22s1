import Map1

-- costo: O(n^2) 
-- Ejercicio 5
-- Implemente estas otras funciones como usuario de Map:
indexar :: [a] -> Map Int a
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar []     = emptyM
indexar (x:xs) = assocM x (length xs) (indexar xs)

-- costo: O(n^2) 
ocurrencias :: String -> Map Char Int
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias []     = emptyM
ocurrencias (x:xs) = assocM x (1 + contarRepetidos x xs) (ocurrencias (eliminarOcurrenciasDe x xs))

-- costo: O(n)
contarRepetidos :: Eq a => a -> [a] -> Int
contarRepetidos a []     = 0
contarRepetidos a (x:xs) = 
    if x == a
        then 1 + contarRepetidos a xs
        else contarRepetidos a xs

-- costo: O(n)
eliminarOcurrenciasDe :: Eq a => a -> [a] -> [a]
eliminarOcurrenciasDe a []     = []
eliminarOcurrenciasDe a (x:xs) = 
    if x == a
        then eliminarOcurrenciasDe a xs
        else x : eliminarOcurrenciasDe a xs        

-- Indicar los ordenes de complejidad en peor caso de cada función de la interfaz y del usuario.
