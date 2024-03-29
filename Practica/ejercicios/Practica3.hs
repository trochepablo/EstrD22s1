module Practica3 where

import Practica1 ( unoSi )

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
bolitaUnaDeCada = Bolita Azul (Bolita Rojo CeldaVacia)

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia    = 0
nroBolitas c (Bolita cb b) = unoSi (sonMismoColor c cb) + nroBolitas c b

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True 
sonMismoColor Rojo Rojo = True
sonMismoColor _    _    = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c bolita = (Bolita c bolita)

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia    = CeldaVacia
sacar c (Bolita cb b) = 
    if sonMismoColor c cb 
        then b
        else Bolita cb (sacar c b)

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n c celda = poner c (ponerN (n-1) c celda) 


-- 1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino



tesorosEntre = Nada (
                    Nada (
                        Nada (
                            Cofre [Tesoro] (
                                Nada (
                                    Cofre [Tesoro, Tesoro] Fin)
                                    )
                            )
                        )
                    )

                    

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin               = False
hayTesoro (Nada camino)     = hayTesoro camino
hayTesoro (Cofre xs camino) = hayTesoroEnObjetos xs || hayTesoro camino

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True
esTesoro _      = False

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin               = 0
pasosHastaTesoro (Nada camino)     = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre xs camino) = 
    if hayTesoroEnObjetos xs
        then 0
        else 1 + pasosHastaTesoro camino

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
-- precondicion: La cantidad de pasos tiene que ser mayor a cero
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino                     = hayTesoroEnCamino camino
hayTesoroEn _ Fin                        = False
hayTesoroEn pasos (Nada camino)          = hayTesoroEn (pasos - 1) camino
hayTesoroEn pasos (Cofre objetos camino) = hayTesoroEn (pasos - 1) camino

hayTesoroEnCamino :: Camino -> Bool 
hayTesoroEnCamino (Cofre xs _) = hayTesoroEnObjetos xs
hayTesoroEnCamino _            = False 

-- Indica si hay al menos “n” tesoros en el camino.
-- precondicion: la cantidad de tesoro n debe ser mayor a cero
alMenosNTesoros :: Int -> Camino -> Bool 
alMenosNTesoros 0 camino                   = True
alMenosNTesoros nTesoros Fin               = False
alMenosNTesoros nTesoros (Nada camino)     = alMenosNTesoros nTesoros camino
alMenosNTesoros nTesoros (Cofre xs camino) = alMenosNTesoros (restarTesorosA nTesoros xs) camino

restarTesorosA :: Int -> [Objeto] -> Int
restarTesorosA nTesoros obs = 
    if (nTesoros - cantTesorosEn obs) < 0
        then 0
        else nTesoros - cantTesorosEn obs

-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.

-- Precondicion: La cantidad de pasos hasta (n1) debe ser mayor a cantidad de pasos Desde (n2)
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0  n2 camino            = contarTesorosHasta n2 camino
cantTesorosEntre _  _  Fin               = 0
cantTesorosEntre n1 n2 (Nada camino)     = cantTesorosEntre (n1-1) (n2-1) camino
cantTesorosEntre n1 n2 (Cofre xs camino) = cantTesorosEntre (n1-1) (n2-1) camino

contarTesorosHasta :: Int -> Camino -> Int
contarTesorosHasta 0 camino            = contarTesorosSiEsCofre camino
contarTesorosHasta _ Fin               = 0
contarTesorosHasta n (Nada camino)     = contarTesorosHasta (n-1) camino
contarTesorosHasta n (Cofre xs camino) = cantTesorosEn xs + contarTesorosHasta (n-1) camino

contarTesorosSiEsCofre :: Camino -> Int 
contarTesorosSiEsCofre (Cofre xs _) = cantTesorosEn xs
contarTesorosSiEsCofre _            = 0

cantTesorosEn :: [Objeto] -> Int
cantTesorosEn []     = 0
cantTesorosEn (x:xs) = unoSi (esTesoro x) + cantTesorosEn xs


caminoConDeTodo = Cofre [Cacharro, Cacharro] (Nada (Cofre [Cacharro,Tesoro] Fin))

caminoConCacharro = Cofre [Cacharro] (Nada (Cofre [Cacharro, Cacharro] Fin))

caminoConTesoros = Nada (Nada (Nada (Cofre [Tesoro, Tesoro] Fin)))

caminoCon1Tesoro = Cofre [Tesoro] (Nada Fin)

-- 2 --
--2.1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- 1. 
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2

--2. 
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
-- en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

-- 3. 
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT x t1 t2) = NodeT (x*2) (mapDobleT t1) (mapDobleT t2)

-- 4. 
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT          = False
perteneceT e (NodeT x t1 t2) = x == e || perteneceT e t1 || perteneceT e t2 

-- 5. 
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
-- iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT           = 0
aparicionesT e (NodeT x t1 t2)  = unoSi (x==e) + aparicionesT e t1 + aparicionesT e t2

-- 6. 
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.

leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x t1 t2) = agregarSiEsHoja x t1 t2 ++ leaves t1 ++ leaves t2

agregarSiEsHoja :: a -> Tree a -> Tree a -> [a]
agregarSiEsHoja e EmptyT EmptyT = [e]
agregarSiEsHoja e _      _      = []

-- 7. 
-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol1
-- . La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

-- 8. 
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

-- 9. 
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = leaves t1 ++ x : leaves t2

node1 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 (NodeT 4 EmptyT (NodeT 5 EmptyT (NodeT 6 EmptyT EmptyT))) EmptyT)

-- 10. 
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
-- nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
-- distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- 11. 
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
-- dicho árbol.
--
-- listPerLevel :: Tree a -> [[a]]
-- listPerLevel nodo = elementosPorLevel nodo (heightT nodo)

-- elementosPorLevel :: Tree a -> Int -> [[a]]
-- elementosPorLevel nodo 0 = [levelN 0 nodo]
-- elementosPorLevel nodo n = levelN n nodo : elementosPorLevel nodo (n-1)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss       = yss
juntarNiveles xss      []        = xss
juntarNiveles (xs:xss) (ys: yss) = (xs ++ ys) : juntarNiveles xss yss


-- Devuelve los elementos de la rama más larga del árbol
-- 12. 
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : elegirRamaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

elegirRamaMasLarga :: [a] -> [a] -> [a]
elegirRamaMasLarga lvs1 lvs2 = if length lvs1 > length lvs2
                                    then lvs1
                                    else lvs2 

-- 13. 
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

--1. 
eval :: ExpA -> Int
eval (Valor n)         = n 
eval (Sum expA expB)   = eval expA + eval expB
eval (Prod expA expB)  = eval expA * eval expB
eval (Neg expA)        = eval expA * (-1)

-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):

simplificar :: ExpA -> ExpA
simplificar (Valor n)    = Valor n
simplificar (Sum expA expB)  = simplificarSum (simplificar expA) (simplificar expB)
simplificar (Prod expA expB) = simplificarProd (simplificar expA) (simplificar expB)
simplificar (Neg expA)      = simplificarNeg (simplificar expA)


simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) expB   = expB
simplificarSum expA (Valor 0)   = expA
simplificarSum expA expB        = Sum expA expB

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) expB      = Valor 0
simplificarProd expA (Valor 0)      = Valor 0
simplificarProd (Valor 1) expB      = expB
simplificarProd expA (Valor 1)      = expA
simplificarProd expA expB           = Prod expA expB

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg expA) = expA
simplificarNeg expA       = (Neg expA)
