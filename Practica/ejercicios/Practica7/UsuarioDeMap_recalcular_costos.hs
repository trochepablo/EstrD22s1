import Map1


-- 1. 
-- justificacion: porque en peor caso obtenerValores tiene costo K log K <<O(K log K)>> 
-- y absorbe el costo lineal de domM
valuesM :: Ord k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
-- costo: O(K log K) siendo K la cantidad de claves en el map
valuesM m = obtenerValores (domM m) m -- costo O(K)

-- justificacion: por cada K realiza, en peor caso, lookupM que es costo O(log K) y solo una operacion constante (:)
obtenerValores :: Ord k => [k] -> Map k v -> [Maybe v]
-- proposito: buscar mediante k el valor asociado en m
-- costo: O(K log K) Por cada clave realiza una busqueda en map para obtener su valor
obtenerValores []     m = []
obtenerValores (k:ks) m = lookupM k m : obtenerValores ks m
                        -- O(log K)
-- 2. 
-- justificacion: por cada elemento de la lista k hace una operación logaritmica lookupM y dos operaciones constantes (PM hay valor y &&)
todasAsociadas :: Ord k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
-- costo: O(log K) siendo K la cantidad de claves en el mapa.
todasAsociadas []     m = True    
todasAsociadas (k:ks) m = hayValor (lookupM k m) && todasAsociadas ks m

hayValor :: Maybe v -> Bool
hayValor Nothing = False
hayValor _       = True

-- 3. 
-- justificacion: porque assocM es de costo O(K * log K)
listToMap :: Ord k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
-- costo: O(N log K) siendo K la cantidad de claves del map y N la lista de tuplas clave-map a asociar en map
listToMap []           = emptyM
listToMap ((k,v): kvs) = assocM k v (listToMap kvs)


-- 4. 
-- justificacion: porque armarvaloresPorKey es de costo O(K * log K)
mapToList :: Ord k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
-- costo: O(K * log K) siendo K la cantidad de claves del map
mapToList m = armarvaloresPorKey (domM m) m

-- justificacion: por cada elemento de la lista k realizo una operacion logaritmica <<lookupM>> y una operacion constante <<:>>
armarvaloresPorKey :: Ord k => [k] -> Map k v -> [(k, v)]
-- proposito: por cada clave del map arma una lista de tuplas clave-valor
-- costo: O(K * log K) siendo K la cantidad de claves del map
armarvaloresPorKey []     m = []
armarvaloresPorKey (k:ks) m = (k, fromJust (lookupM k m)) : armarvaloresPorKey ks m

-- costo: O(1)
fromJust :: Maybe v -> v
fromJust (Just valor) = valor
fromJust _ = Nothing



-- 5.
-- justificacion: 
agruparEq :: Ord k => [(k, v)] -> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
-- costo: 
agruparEq []          = emptyM 
agruparEq ((k,v):kvs) = assocM k (v : (obtenerValoresDeK k kvs)) (agruparEq kvs)
--                       O(log K)       


-- costo: O(K)
obtenerValoresDeK :: Eq k => k -> [(k,v)] -> [v]
obtenerValoresDeK key []           = []
obtenerValoresDeK key ((k,v):kvs) = 
    if key == k 
        then v : obtenerValoresDeK key kvs 
        else obtenerValoresDeK key kvs

-- la misma clave.
-- 6. 


-- justificar
incrementar :: Ord k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
--
incrementar []     m = emptyM
incrementar (k:ks) m = assocM k (fromJust (lookupM k m)+1) (incrementar ks m)


-- emptyM :: Map k v
-- Costo: O(1).
-- assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Costo: O(log K).
-- lookupM :: Ord k => k -> Map k v -> Maybe v
-- Costo: O(log K).
-- deleteM :: Ord k => k -> Map k v -> Map k v
-- Costo: O(log K).
-- domM :: Map k v -> [k]
-- domM: O(K).


-- 7. 

-- justificacion: porque en peor de los casos agregarValoresEn es costo O(N * (log N + log K)) 
-- y absorbe a domM de costo lineal
mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
-- costo: O(N * (log N + log K)) siendo N la cantidad de claves del primer map y K las del segundo map
mergeMaps map1 map2 = agregarValoresEn (domM map1) map1 map2


-- justificacion: porque por cada elemento de N realiza una operacion O(log K) <<assocM>> 
-- y tambien opera con <<obtenerValorPorClave>> de costo O(log N)
agregarValoresEn :: Ord k => [k] -> Map k v -> Map k v -> Map k v
-- costo: O(N * (log N + log K)) siendo N la cantidad de claves del primer map y K las del segundo map
agregarValoresEn []     m1 m2 = m2 
agregarValoresEn (k:ks) m1 m2 = assocM k (obtenerValorPorClave k m1) (agregarValoresEn ks m1 m2)
                            --  O(log K)            O(log N)

-- justificacion: siendo lookupM el peor caso posible O(log N) y fromjust como operacion constante (PM)
obtenerValorPorClave :: Ord k => k -> Map k v -> v
-- proposito:
-- costo: O(log N) siendo N las claves del map 
obtenerValorPorClave k m1 = fromJust (lookupM k m1)
--                                    O(log N)
