module Map1 (Map, emptyM, assocM, lookupM, deleteM, domM) where

data Map k v = MP [(k, v)]
{-
    INV.REP: En (MP kvs) se cumple:
        * en kvs no hay claves repetidas
-}

emptyM :: Map k v
-- Propósito: devuelve un map vacío
emptyM = MP []

-- costo: O(n)
assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM key value (MP kvs) = MP (agregar key value kvs)

-- costo: O(n)
agregar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
-- proposito: agrega v en el diccionario de kvs cuando key y la k de kvs coinciden
-- proposito: agrega el valor v con la clave k en el diccionario 
agregar key value []          = [(key, value)]
agregar key value ((k,v):kvs) = 
    if key == k 
        then (k, value):kvs 
        else (k,v) : agregar key value kvs

-- costo: O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM key (MP kvs) = buscar key kvs

-- costo: O(n)
buscar :: Eq k => k -> [(k,v)] -> Maybe v
-- proposito: busca el valor de la clave k en el disccionario kvs
buscar key []          = Nothing
buscar key ((k,v):kvs) = 
    if key == k
        then Just v
        else buscar key kvs

-- costo: O(n)
deleteM :: Ord k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM key (MP kvs) = MP (borrar key kvs)

-- costo: O(n)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
-- proposito: elimina la clave k en el disccionario kvs
borrar key []          = []
borrar key ((k,v):kvs) = 
    if key == k
        then kvs
        else (k,v) : borrar key kvs

-- costo: O(n)
domM :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
domM (MP kvs) = clavesDe kvs

-- costo: O(n)
clavesDe :: [(k,v)] -> [k]
-- proposito : devuelve todas las claves de lista de tuplas de k,v
clavesDe []          = []
clavesDe ((k,_):kvs) = k : clavesDe kvs 