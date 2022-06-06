data RAList a = MkR Int (Map Int a) (Heap a)
                {- Inv.Rep: En (MkR posicion indiceValores valores) se cumple que:
                    * el heap tiene la misma cantidad de valores que map
                    * al agregar un elemento a map la clave es la posicion
                -}

-- Un Int, que representa la próxima posición a ocupar en la lista. Es decir, cuando se agregue un elemento al final, debe
-- agregarse en dicha posición, que luego será incrementada. Cuando la estructura está vacía, el número es 0.
-- Un Map Int a, que representa la relación entre índices (claves) y valores de la estructura.
-- Una Heap a que contiene todos los valores de la estructura.


-- a) 
emptyRAL :: RAList a
-- Propósito: devuelve una lista vacía.
-- Eficiencia: O(1).
emptyRAL = MkR 0 emptyM emptyH

-- b) 
isEmptyRAL :: RAList a -> Bool
-- Propósito: indica si la lista está vacía.
-- Eficiencia: O(1).
isEmptyRAL (MkR posicion _ _) = posicion == 0

-- c) 
lengthRAL :: RAList a -> Int
-- Propósito: devuelve la cantidad de elementos.
-- Eficiencia: O(1).
lengthRAL (MkR posicion _ _) = posicion

-- d) 
get :: Int -> RAList a -> a
-- Propósito: devuelve el elemento en el índice dado.
-- Precondición: el índice debe existir.
-- Eficiencia: O(log N).
get indice (MkR _ map _) = fromJust (lookupM indice map)

-- e) 
minRAL :: Ord a => RAList a -> a
-- Propósito: devuelve el mínimo elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(1).
minRAL (MkR _ map valores) = findMin valores


-- f) add :: Ord a => a -> RAList a -> RAList a
-- Propósito: agrega un elemento al final de la lista.
-- Eficiencia: O(log N).
add elemento (MkR posicion map heap) = MkR (posicion+1) (assocM posicion elemento map) (insertH elemento heap)

-- g) 
elems :: Ord a => RAList a -> [a]
-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
-- Eficiencia: O(N log N).
elems (MkR posicion map heap) = extraerElementos posicion map

extraerElementos :: Ord a => Int -> Map Int a -> [a]
extraerElementos 0 map = []
extraerElementos n map = fromJust (lookupM (n-1) map) : extraerElementos (n-1) map

-- h) 
remove :: Ord a => RAList a -> RAList a
-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove (MkR posicion map heap) = case lookupM (posicion-1) map of
    (Just x) -> MkR (i-1) (deleteM x map) (deleteH x heap)
    Nothing -> Error "no cumple precondicion"


-- justificacion: hay que ir recorriendo la heap realizando operaciones constantes O(1) (findMin, ==)
-- y logaritmicas O(log N) (deleteMin, insertH)
deleteH :: Ord a => a -> Heap a -> Heap a
-- proposito: elimina el elemento x de la heap
-- costo: O(N log N) siendo N la cantidad de valores a en la heap
deleteH x h =
    if x == findMin h
        then deleteMin h
        else insertH (findMin h) (deleteH x (deleteMin h))    

-- i) 
set :: Ord a => Int -> a -> RAList a -> RAList a
-- Propósito: reemplaza el elemento en la posición dada.
-- Precondición: el índice debe existir.
-- Eficiencia: O(N log N).
set i e (MkR posicion map heap) = let value = lookupM i map in
    MkR (posicion-1) (asoccM i e map) (insertH e (deleteH value heap))


-- j) 
addAt :: Ord a => Int -> a -> RAList a -> RAList a
-- Propósito: agrega un elemento en la posición dada.
-- Precondición: el índice debe estar entre 0 y la longitud de la lista.
-- Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- Eficiencia: O(N log N).
-- Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
-- también como argumento la máxima posición posible.
addAt i e (MkR p map heap) = MkR (p+1) (insertarEnP 0 i e p map) (insertH e)

insertarEnP :: Ord a => Int -> Int -> Int -> a -> Map k a -> Map k a
insertarEnP i f e p map = 
    if (i == f)
        then assocM i e (insertarDesdeP i p map)
        else assocM (fromJust (lookupM i map)) (insertarEnP (i+1) f p map)

insertarDesdeP :: Ord a => Int -> Int -> Map k a        
insertarDesdeP i f map =
    if i == f 
        then map
        else assocM (i+1) (lookupM i map) (insertarDesdeP (i+1) f map)