import Map1


-- 1. 
valuesM :: Ord k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = obtenerValues (domM m) m

-- costo: O(n)
obtenerValues :: Ord k => [k] -> Map k v -> [Maybe v]
-- proposito: buscar mediante k el valor asociado en m
obtenerValues []     m = []
obtenerValues (k:ks) m = lookupM k m : obtenerValues ks m

-- 2. 
-- costo: O(n)
todasAsociadas :: Ord k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas []     m = True    
todasAsociadas (k:ks) m = hayValor (lookupM k m) && todasAsociadas ks m

hayValor :: Maybe v -> Bool
hayValor Nothing = False
hayValor _       = True

-- 3. 
listToMap :: Ord k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap []           = emptyM
listToMap ((k,v): kvs) = assocM k v (listToMap kvs)


-- 4. 
-- costo: (n)
mapToList :: Ord k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList m = []
mapToList m = armarvaloresPorKey (domM m) m

armarvaloresPorKey :: Ord k => [k] -> Map k v -> [(k, v)]
armarvaloresPorKey []     m = []
armarvaloresPorKey (k:ks) m = (k, fromJust (lookupM k m)) : armarvaloresPorKey ks m

fromJust :: Maybe v -> v
fromJust (Just valor) = valor
fromJust _ = error ""


-- 5.
agruparEq :: Ord k => [(k, v)] -> Map k [v]
--agruparEq = error ""
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
agruparEq []          = emptyM 
agruparEq ((k,v):kvs) = assocM k (v : (obtenerValoresDeK k kvs)) (agruparEq kvs)



-- (v : (obtenerValoresDeK k kvs))
obtenerValoresDeK :: Eq k => k -> [(k,v)] -> [v]
obtenerValoresDeK key []           = []
obtenerValoresDeK key ((k,v):kvs) = 
    if key == k 
        then v : obtenerValoresDeK key kvs 
        else obtenerValoresDeK key kvs

-- la misma clave.
-- 6. 

incrementar :: Ord k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar []     m = emptyM
incrementar (k:ks) m = assocM k (fromJust (lookupM k m)+1) (incrementar ks m)

-- 7. 
mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps map1 map2 = agregarValoresEn (domM map1) map1 map2

agregarValoresEn :: Ord k => [k] -> Map k v -> Map k v -> Map k v
agregarValoresEn []     m1 m2 = m2 
agregarValoresEn (k:ks) m1 m2 = assocM k (obtenerValorPorClave k m1) (agregarValoresEn ks m1 m2)

obtenerValorPorClave :: Ord k => k -> Map k v -> v
obtenerValorPorClave k m1 = fromJust (lookupM k m1)