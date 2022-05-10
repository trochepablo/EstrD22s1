module Map3 (Map, emptyM, assocM, lookupM, deleteM, domM) where

data Map k v = MP [k] [v]
{-
    INV.REP: En (MP ks vs) se cumple que:
        * la posición i de ks corresponde a la posición i de vs
        * ks y vs tiene la misma longitud.
-}

emptyM :: Map k v
-- Propósito: devuelve un map vacío
emptyM = MP [] []

-- costo: O(n)
assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM key value (MP ks vs) = MP (key:ks) (value:vs)

-- -- costo: O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v
-- -- Propósito: encuentra un valor dado una clave.
lookupM key (MP ks vs) = buscar key ks vs

-- costo: O(n)
buscar :: Eq k => k -> [k] -> [v] Maybe v
-- proposito: busca el valor de la clave k en el disccionario kvs
buscar key []     _      = Nothing
buscar key (k:ks) (v:vs) = 
    if key == k
        then Just v
        else buscar key ks vs

-- costo: O(n)
deleteM :: Ord k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM key (MP ks vs) = MP (borrarK key ks) (borrarV key ks vs)

-- costo: O(n)
borrarK :: Eq k => k -> [k] -> [k]
-- proposito: elimina la clave k en el disccionario kvs
borrar key []     = []
borrar key (k:ks) =  
    if key == k
        then borrar key ks
        else k : borrar key ks

-- costo: O(n)
borrarK :: Eq k => k -> [k] -> [v] -> [v]
-- proposito: elimina la clave k en el disccionario kvs
borrar key []     _      = []
borrar key (k:ks) (v:vs) =  
    if key == k
        then borrar key ks vs
        else v : borrar key ks

-- costo: O(1)
domM :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
domM (MP ks vs) = ks