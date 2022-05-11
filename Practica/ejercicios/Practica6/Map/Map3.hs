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
-- costo: O(1) porque solo agrega un elemento al map
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

deleteM :: Ord k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
-- costo: O(N)
deleteM key (MP ks vs) = MP (borrarK key ks) (borrarV key ks vs)

-- costo: O(n)
borrarK :: Eq k => k -> [k] -> [k]
-- proposito: elimina la clave k en el diccionario kvs
borrarK key []     = []
borrarK key (k:ks) =  
    if key == k
        then borrarK key ks
        else k : borrarK key ks

-- costo: O(n)
borrarV :: Eq k => k -> [k] -> [v] -> [v]
-- proposito: elimina la clave k en el diccionario kvs
borrarV key []     _      = []
borrarV key (k:ks) (v:vs) =  
    if key == k
        then borrarV key ks vs
        else v : borrarV key ks

-- costo: O(N^2) siendo N la longitud de ks
domM :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
domM (MP ks vs) = sinRepetidos ks

sinRepetidos :: Eq k => [k] -> [k]
sinRepetidos []     = []
sinRepetidos (k:ks) = agregarSiPertenece k (sinRepetidos ks)

-- costo: O(n) 
agregarSiPertenece :: Eq k => k -> [k] -> [k]
agregarSiPertenece k ks = if elem k ks then ks else k:ks
