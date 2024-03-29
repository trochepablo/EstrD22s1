-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:
-- emptyS :: Set a
-- Crea un conjunto vacío.
-- addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
-- Página 2 de 5
-- Estructuras de datos - UNQ
-- removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
-- unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- 1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.
-- Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
-- de esta implementación, pero para mantener una interfaz común entre distintas posibles
-- implementaciones estamos obligados a escribir así los tipos.
-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
-- losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
-- sinRepetidos :: Eq a => [a] -> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
-- 3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
-- otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
-- sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
-- por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.