import Practica1

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia


-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia    = 0
nroBolitas c (Bolita cb b) = unoSi (sonMismoColor c cb)

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True 
sonMismoColor Rojo Rojo = True
sonMismoColor _    _    = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c CeldaVacia    = Bolita c CeldaVacia
poner c (Bolita cb b) = Bolita cb (Bolita c CeldaVacia)

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
ponerN n c CeldaVacia = agregarNCantBolitas n c
ponerN n c (Bolita cb b) = Bolita cb (ponerN n c b)

agregarNCantBolitas :: Int -> Color -> Celda
agregarNCantBolitas 0 _ = CeldaVacia
agregarNCantBolitas n c = Bolita c (agregarNCantBolitas (n-1) c)

-- 1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

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
        then pasosHastaTesoro Fin
        else 1 + pasosHastaTesoro camino

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin     = False
hayTesoroEn 0 camino  = hayTesoroAqui camino
hayTesoroEn n camino  = hayTesoroEn (n-1) (siguienteCamino camino)

siguienteCamino :: Camino -> Camino
siguienteCamino Fin              = Fin   
siguienteCamino (Nada camino)    = camino
siguienteCamino (Cofre _ camino) = camino

hayTesoroAqui :: Camino -> Bool
hayTesoroAqui Fin          = False
hayTesoroAqui (Nada _)     = False
hayTesoroAqui (Cofre xs _) = hayTesoroEnObjetos xs

-- Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin    = False
alMenosNTesoros 0 _      = True 
alMenosNTesoros n camino = alMenosNTesoros (restarCantTesorosAn n camino) camino

restarCantTesorosAn :: Int -> Camino -> Int
restarCantTesorosAn n Fin               = n
restarCantTesorosAn n (Nada _)          = n
restarCantTesorosAn n (Cofre xs camino) = 
    let cantTesoros = cantTesorosEn xs in
    if cantTesoros < 0
        then 0
        else cantTesoros

cantTesorosEn :: [Objeto] -> Int
cantTesorosEn []     = 0
cantTesorosEn (x:xs) = unoSi (esTesoro x) + cantTesorosEn xs


-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _  _  Fin             = 0
cantTesorosEntre n1 n2 camino | n1 < 0 = 0
cantTesorosEntre 0  n2 camino          = contarTesorosHasta n2 camino
cantTesorosEntre n1 n2 camino          = cantTesorosEntre (n1-1) n2 (siguienteCamino camino)

contarTesorosHasta :: Int -> Camino -> Int
contarTesorosHasta _ Fin            = 0
contarTesorosHasta n camino | n < 0 = 0
contarTesorosHasta 0 camino         = cantDeTesorosEnCamino camino
contarTesorosHasta n camino         = cantDeTesorosEnCamino camino + contarTesorosHasta (n-1) (siguienteCamino camino)

cantDeTesorosEnCamino :: Camino -> Int 
cantDeTesorosEnCamino (Cofre xs _) = cantTesorosEn xs
cantDeTesorosEnCamino _            = 0

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
leaves (NodeT x t1 t2) = x : leaves t1 ++ leaves t2

-- 7. 
-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol1
-- . La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 
    if (sizeT t1) > (sizeT t2)
        then heightT t1
        else heightT t2 

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

node1 = NodeT 1 (NodeT 2 (NodeT 4 EmptyT EmptyT) (NodeT 5 EmptyT EmptyT)) (NodeT 3 EmptyT EmptyT)

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
listPerLevel :: Tree a -> [[a]]


-- 12. ramaMasLarga :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol
-- 13. todosLosCaminos :: Tree a -> [[a]]
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.



-- data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- todosLosCaminos2 :: Tree a -> [[a]]
-- todosLosCaminos2 EmptyT          = []
-- todosLosCaminos2 (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos2 t1) ++ consACada x (todosLosCaminos2 t2)

-- consACada :: a -> [[a]] -> [[a]]
-- consACada x []       = []
-- consACada x (xs:xss) = (x:xs) : consACada x xss

-- tree1 = 
--     NodeT 1 
--     (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT)
--     (NodeT 3 EmptyT EmptyT)


data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

--1. 
eval :: ExpA -> Int
eval (Valor n)         = n 
eval (Sum expA expB)   = eval expA + eval expB
eval (Prod expA expB)  = eval expA * eval expB
eval (Neg expA)        = eval expA * (-1)


-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):
-- simplificar :: ExpA -> ExpA
-- simplificar (Sum expA expB) = 
--     if esCero expA
--         then expB
--         else eval (Sum expA expB)

-- simplificar (Prod expA expB) = 
--     if esCero expA || esCero expB
--         then 0
--         else eval (Prod expA expB)

esCero :: ExpA -> Bool
esCero (Valor n) = n == 0
esCero _         = False

-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x