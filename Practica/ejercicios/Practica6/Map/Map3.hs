module Map3 (Map, emptyM, assocM, lookupM, deleteM, domM) where

data Map k v = MP [k] [v]
{-
    INV.REP: En (MP ks vs) se cumple que:
        * ks y vs tiene la misma longitud.

    observacion: la posición i de ks corresponde a la posición i de vs
-}

-- justificacion: realiza una operaciones constantes (MP [] []) para instanciar la estructura
-- con MP y listas vacias
emptyM :: Map k v
-- Propósito: devuelve un map vacío
-- costo: O(1) porque opera sobre map vacio
emptyM = MP [] []

-- justificacion: realiza operaciones constantes (MP cons) para construir map 
-- y agregar elemento a cada lista
assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
-- costo: O(1) porque solo agrega un clave-valor al map
assocM key value (MP ks vs) = MP (key:ks) (value:vs)

-- justificacion: buscar es O(N)
lookupM :: Ord k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
-- costo: O(N) Siendo N las veces que se hizo assocM
lookupM key (MP ks vs) = buscar key ks vs

-- justificacion: por cada elemento evalua igualdad key con k O(1) y extrae valor con Just O(1) 
buscar :: Eq k => k -> [k] -> [v] -> Maybe v
-- proposito: busca el valor de la clave k en el diccionario kvs
-- precondicion: las litas no pueden ser vacias.
-- costo: O(N) Siendo N la longitud de la lista de k
buscar key []     _      = Nothing
buscar key (k:ks) (v:vs) = 
    if key == k
        then Just v
        else buscar key ks vs

-- justificacion: borrarK y borrarV son O(N)
deleteM :: Ord k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
-- costo: O(N) siendo N la cantidad de claves en el map
deleteM key (MP ks vs) = MP (borrarK key ks) (borrarV key ks vs)

-- justificacion: por cada elemento de la lista de k se compara igualdad con key 
-- y cons para agregar k a la lista
borrarK :: Eq k => k -> [k] -> [k]
-- proposito: elimina la clave k en el diccionario kvs
-- costo: O(N) siendo N la longitud de la lista de k
borrarK key []     = []
borrarK key (k:ks) =  
    if key == k
        then borrarK key ks
        else k : borrarK key ks

-- justificacion: por cada elemento de la lista de k se compara igualdad con key 
-- y cons para agregar v a la lista 
borrarV :: Eq k => k -> [k] -> [v] -> [v]
-- proposito: elimina la clave k en el diccionario kvs
-- costo: O(N) siendo N la longitud de la lista de k
borrarV key []     _      = []
borrarV key (k:ks) (v:vs) =  
    if key == k
        then borrarV key ks vs
        else v : borrarV key ks

-- justificacion: sinRepetidos es O(N^2)
domM :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
-- costo: O(N^2) siendo N la longitud de ks
domM (MP ks vs) = sinRepetidos ks

-- justificacion: por cada elemento de la lista de k realizo agregarSiPertenece O(N)
sinRepetidos :: Eq k => [k] -> [k]
-- proposito: devuelve una lista de k sin repetidos
-- costo: O(N^2) siendo N la longitud de la lista de k
sinRepetidos []     = []
sinRepetidos (k:ks) = agregarSiPertenece k (sinRepetidos ks)

-- justificar: 
agregarSiPertenece :: Eq k => k -> [k] -> [k]
-- proposito: agrega a la lista el elemento k si no pertenece
-- costo: O(N) siendo N la cantidad de elementos de ks
agregarSiPertenece k ks = if elem k ks then ks else k:ks
