module MultiSet1 (MultiSet, emptyMS) where --, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList

import Map1

data MultiSet a = MS (Map a Int) deriving Show

emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS elemento (MS map) = let elemBuscado = lookupM elemento map in
    if elemBuscado == Nothing
        then MS (assocM elemento 1 map)
        else MS (assocM elemento ((fromJust elemBuscado) + 1) map)

--addMS "a" (addMS "b" (addMS "a" emptyMS))


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS elemento (MS map) =  fromJust (lookupM elemento map)
-- ocurrencesMS "a"  (addMS "a" (addMS "b" (addMS "a" emptyMS)))

-- unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.

-- intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.

multiSetToList :: Ord a => MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList (MS map) = obtenerValoresDeK (domM map) map

-- === auxiliares ===

fromJust :: Maybe Int -> Int
fromJust (Just valor) = valor
fromJust _ = 0

obtenerValoresDeK :: Ord a => [a] -> Map a Int -> [(a, Int)]
obtenerValoresDeK [] map     = []
obtenerValoresDeK (k:ks) map = agregar k (lookupM k map) : obtenerValoresDeK ks map

agregar :: a -> Maybe Int -> (a, Int)
agregar key value = (key, fromJust value)