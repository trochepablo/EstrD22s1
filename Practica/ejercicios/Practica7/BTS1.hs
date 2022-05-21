-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
-- invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
-- desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
-- del árbol. Justificar por qué la implementación satisface los costos dados.

module BTS1 (BTS, belongsBST) where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

data BTS a = ConsBST (Tree a) deriving Show
{-
    INV. REP.: en (ConsBST t) t cumple ser un BST y estar balanceado
-}-

-- 1. 
-- justificacion: Por cada nodo de t se realiza una operación constante (==) 
-- al estar ordenada t no se recorre todo el arbol
belongsBST :: Ord a => a -> Tree a -> Bool
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N) siendo N la cantidad de nodos del arbor BST balanceado
belongsBST elemento EmptyT          = False
belongsBST elemento (NodeT x ti td) =
    if elemento == x then True
        else if (elemento > x)
            then belongsBST elemento td 
            else belongsBST elemento ti

-- 2. 
-- justificacion: Por cada nodo de t se realiza una operación constante (>) 
-- al estar ordenada t no se recorre todo el arbol
insertBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST elemento EmptyT          = NodeT elemento EmptyT EmptyT
insertBST elemento (NodeT x ti td) = 
    if elemento < x 
        then NodeT x (insertBST elemento ti) td
        else NodeT x ti (insertBST elemento td)

-- 3. 
deleteBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)
deleteBST elemento EmptyT          = EmptyT
deleteBST elemento (NodeT x ti td) = 
    if elemento < x then rearmarBST ti td
    else if (elemento > x)
        then NodeT x (deleteBST elemento ti) td
        else NodeT x ti (deleteBST elemento td)

-- justificacion: avanza por la rama ti (rama menor ordenada) hasta encontrar el máximo
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
-- PRECON: ambos arboles son BST
-- Costo: O(log N) busca el mayor en el arbol izquierdo BST
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti 
                        in NodeT m ti' td

-- 4. 
-- splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- -- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- -- Costo: O(log N)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td)     = let (m, ti') = splitMinBST ti
                                    in (m, NodeT x ti' td)

arbolBalanceado = NodeT 10 
    (NodeT 5 (NodeT 4 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT)) 
    (NodeT 8 EmptyT (NodeT 9 EmptyT EmptyT))

-- 5. 
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
-- PRECON: el arbol debe ser BST y no debe estar vacio.
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td) = let (m, td') = splitMaxBST td
                                in (m, NodeT x ti td')

    --                             NodeT 10 
    -- (NodeT 5 (NodeT 4 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT)) 
    -- (NodeT 8 EmptyT (NodeT 9 EmptyT EmptyT))

    -- maxSplit (NodeT 8 EmptyT (NodeT 9 EmptyT EmptyT)) => (9, NodeT 8 EmptyT EmptyT)

    -- ti => (NodeT 5 (NodeT 4 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT))
    -- td => (NodeT 8 EmptyT (NodeT 9 EmptyT EmptyT))
    -- t  => NodeT 10 ti td

    -- splitMax td => (NodeT 8 EmptyT (NodeT 9 EmptyT EmptyT)) =

    -- (9 , NodeT 8 EmptyT EmptyT)
    --  |              \                 
    -- (9 , NodeT 10 ti (NodeT 8 EmptyT EmptyT))


    -- ti => EmptyT
    -- td => NodeT 9 EmptyT EmptyT
    -- t  => NodeT 8 ti td

    -- splitMax td => (NodeT 9 EmptyT EmptyT) =

    -- (9 ,               EmptyT)
    --  |                    \                 
    -- (9 , NodeT 8 EmptyT EmptyT))


-- 6. 
esBST :: Tree a -> Bool
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N^2)
esBST EmptyT          = True
esBST (NodeT x ti td) = (esMayor x ti && esMenor x td) && esBST ti && esBST td

esMayor :: Ord a -> a -> Tree a -> Bool
esMayor e EmptyT        = True
esMayor e (NodeT x _ _) = e > x

esMenor :: Ord a -> a -> Tree a -> Bool
esMenor e EmptyT        = True
esMenor e (NodeT x _ _) = e < x



-- 7. 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA e EmptyT          = Nothing
elMaximoMenorA e (NodeT x _ td) =
    if e > x 
        then Maybe x
        else elMaximoMenorA td

-- 8. 
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)
elMinimoMayorA e EmptyT          = Nothing
elMinimoMayorA e (NodeT x ti _) =
    if e < x 
        then Maybe x
        else elMinimoMayorA ti

-- 9. 
balanceado :: Tree a -> Bool
-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N^2)
balanceado EmptyT          = True
balanceado (NodeT _ ti td) = let diferenciasDeAlturas = (alturaPorCantElementos ti) - (alturaPorCantElementos td) in
    estaBalanceado diferenciasDeAlturas
    && (balanceado ti) && (balanceado td)

alturaPorCantElementos :: Tree a -> Int
alturaPorCantElementos t = (logBase2 (sizeT t))

logBase2 :: Int -> Int  -- PRECOND: n>0
logBase2 1 = 0
logBase2 n = 1 + logBase2 (div n 2)

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

estaBalanceado :: Int -> Bool
estaBalanceado difAlturas = difAlturas <= 1