module Map3 (Map, emptyM) where --, assocM, lookupM, deleteM, domM

data Map k v = MP [k] [v] deriving Show
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
assocM key value (MP ks vs) = MP (agregarKey key ks) (agregarValue key value (agregarKey key ks) vs)

-- costo: O(n)
agregarKey :: Eq k => k -> [k] -> [k]
-- proposito: agrega v en el diccionario de kvs cuando key y la k de kvs coinciden
-- proposito: agrega el valor v con la clave k en el diccionario 
agregarKey key []     = [key]
agregarKey key (k:ks) = 
    if key == k 
        then k:ks 
        else k : agregarKey key ks

agregarValue :: Eq k => k -> v -> [k] -> [v] -> [v]
-- proposito: agregar value en vs en la misma posición i donde se encontro k en ks
agregarValue key value ks vs = agregarValueEnPosicion (obtenerPosicion key ks) value vs

obtenerPosicion :: Eq k => k -> [k] -> Int
-- proposito: obtiene la posición i donde se encuentra k en ks
-- precondicion: la key existe en ks
obtenerPosicion key []     = error "el elemento key no existe."
obtenerPosicion key (k:ks) =
    if k == key
        then 0
        else 1 + obtenerPosicion key ks

agregarValueEnPosicion :: Int -> v -> [v] -> [v]
-- proposito: agrega el elemento v en la posición i de vs
-- precondicion: la posicion i debe ser válida en la longitud de vs.
agregarValueEnPosicion n value []     = [value]
agregarValueEnPosicion n value (v:vs) = 
    if n == 0
        then value : v : vs
        else v : agregarValueEnPosicion (n-1) value vs

-- -- costo: O(n)
-- lookupM :: Ord k => k -> Map k v -> Maybe v
-- -- Propósito: encuentra un valor dado una clave.
-- lookupM key (MP kvs) = buscar key kvs

-- -- costo: O(n)
-- buscar :: Eq k => k -> [(k,v)] -> Maybe v
-- -- proposito: busca el valor de la clave k en el disccionario kvs
-- buscar key []          = Nothing
-- buscar key ((k,v):kvs) = 
--     if key == k
--         then Just v
--         else buscar key kvs

-- -- costo: O(n)
-- deleteM :: Ord k => k -> Map k v -> Map k v
-- -- Propósito: borra una asociación dada una clave.
-- deleteM key (MP kvs) = MP (borrar key kvs)

-- -- costo: O(n)
-- borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
-- -- proposito: elimina la clave k en el disccionario kvs
-- borrar key []          = []
-- borrar key ((k,v):kvs) = 
--     if key == k
--         then kvs
--         else (k,v) : borrar key kvs

-- -- costo: O(n)
-- domM :: Map k v -> [k]
-- -- Propósito: devuelve las claves del map.
-- domM (MP kvs) = clavesDe kvs

-- -- costo: O(n)
-- clavesDe :: [(k,v)] -> [k]
-- -- proposito : devuelve todas las claves de lista de tuplas de k,v
-- clavesDe []          = []
-- clavesDe ((k,_):kvs) = k : clavesDe kvs 