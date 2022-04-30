import Set1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] conjunto     = []
losQuePertenecen (x:xs) conjunto = 
    if belongs x conjunto
        then x : losQuePertenecen xs conjunto
        else losQuePertenecen xs conjunto

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos lista = setToList (agregarElementosA lista emptyS)

agregarElementosA :: Eq a => [a] -> Set a -> Set a
agregarElementosA [] conjunto     = emptyS
agregarElementosA (x:xs) conjunto = addS x (agregarElementosA xs conjunto)

unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos EmptyT          = emptyS
unirTodos (NodeT e t1 t2) = unionS (unionS e (unirTodos t1)) (unionS e (unirTodos t2))