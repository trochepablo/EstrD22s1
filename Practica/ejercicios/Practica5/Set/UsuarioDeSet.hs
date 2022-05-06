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
sinRepetidos lista = setToList (armarConjuntoDe lista)

armarConjuntoDe :: Eq a => [a] -> Set a
armarConjuntoDe []     = emptyS
armarConjuntoDe (x:xs) = addS x (agregarElementosA xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos EmptyT          = emptyS
unirTodos (NodeT e t1 t2) = unionS (unionS e (unirTodos t1)) (unionS e (unirTodos t2))

losQuePertenecen [2,1] (addS 1 (addS 2 (addS 3 (emptyS))))