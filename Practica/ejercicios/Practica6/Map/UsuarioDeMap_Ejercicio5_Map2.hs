import Map1

-- costo: O(N^2) costo lineal respecto de la lista sin repetidos que provee TAD 
-- Ejercicio 5
-- Implemente estas otras funciones como usuario de Map:
indexar :: [a] -> Map Int a
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar xs = armarMapDe 0 xs

armarMapDe :: [a] -> Map Int a
armarMapDe []     _ = emptyM
armarMapDe (x:xs) n = assocM n x (armarMapDe (n+1) xs)


-- costo: O((n*m)^2) siendo n la logitud de la primer lista y m de la segunda 
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
