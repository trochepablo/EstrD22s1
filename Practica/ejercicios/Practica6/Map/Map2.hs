module Map1 (Map, emptyM, assocM, lookupM, deleteM, domM) where

data Map k v = MP [(k, v)]

emptyM :: Map k v
-- Propósito: devuelve un map vacío
emptyM = MP []

-- costo: O(1)
assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM key value (MP kvs) = MP ((key,value) : kvs)

-- costo: O(N)
lookupM :: Ord k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM key (MP kvs) = buscar key kvs

-- costo: O(N)
buscar :: Eq k => k -> [(k,v)] -> Maybe v
-- proposito: busca el valor de la clave k en el disccionario kvs
buscar key []          = Nothing
buscar key ((k,v):kvs) = 
    if key == k
        then Just v
        else buscar key kvs

-- costo: O(N)
deleteM :: Ord k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM key (MP kvs) = MP (borrar key kvs)

-- costo: O(N)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
-- proposito: elimina la clave k en el disccionario kvs
borrar key []          = []
borrar key ((k,v):kvs) = 
    if key == k
        then borrar key kvs
        else (k,v) : borrar key kvs

-- costo: O(N^2)
domM :: Ord k => Map k v -> [k]
-- Propósito: devuelve las claves del map.
domM (MP kvs) = sinRepetidos (clavesDe kvs)

-- costo: O(N^2)
clavesDe :: [(k,v)] -> [k]
-- proposito : devuelve todas las claves de lista de tuplas de k,v
clavesDe []          = []
clavesDe ((k,_):kvs) = k : clavesDe kvs

-- costo: O(N^2)
sinRepetidos :: Eq k => [k] -> [k]
-- proposito: devuelve una lista del dominio y elimina los repetidos
sinRepetidos []          = []
sinRepetidos (k:ks) =
    if elem k ks -- O(N)
        then k : sinRepetidos (borrarDe k ks)
        else k : sinRepetidos ks

-- costo: O(N)
borrarDe :: Eq k => k -> [k] -> [k]
-- proposito: elimina la clave k en la lista ks
borrarDe key []      = []
borrarDe key (k:kvs) = 
    if key == k
        then borrarDe key kvs
        else k : borrarDe key kvs