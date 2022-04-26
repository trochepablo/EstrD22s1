data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int

--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza    = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) =
    if esIngrediente ing Jamon
        then sacarJamon p
        else Capa ing (sacarJamon p)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Jamon         Jamon         = True
esIngrediente Queso         Queso         = True
esIngrediente Salsa         Salsa         = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _             _             = False

-- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso pizza = tieneIngrediente pizza Salsa && tieneIngrediente pizza Queso

tieneIngrediente :: Pizza -> Ingrediente -> Bool
tieneIngrediente Prepizza     _        = False
tieneIngrediente (Capa ing p) ingTiene = esIngrediente ing ingTiene || tieneIngrediente p ing


-- --Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarCantSiEsAceitunas ing) (duplicarAceitunas p)

duplicarCantSiEsAceitunas :: Ingrediente -> Ingrediente
duplicarCantSiEsAceitunas (Aceitunas cant) = Aceitunas (cant*2)
duplicarCantSiEsAceitunas ingrediente = ingrediente

-- -- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- -- ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs

-- 2. Mapa de tesoros (con bifurcaciones) --

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

mapaCon1Tesoro = Bifurcacion (
                                Cofre [Chatarra]) 
                                    (Bifurcacion 
                                        (Cofre [Chatarra]) 
                                        (Fin 
                                            (Cofre [Chatarra])) 
                                        (Fin (Cofre [Tesoro]))
                                    ) 
                                    (Fin (Cofre [Chatarra]))

-- Definir las siguientes operaciones:
-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre)                   = hayTesoroEnCofre cofre 
hayTesoro (Bifurcacion cofre map1 map2) = hayTesoroEnCofre cofre || hayTesoro map1 || hayTesoro map2

hayTesoroEnCofre :: Cofre -> Bool 
hayTesoroEnCofre (Cofre obs) = hayTesoroEnObjetos obs

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True
esTesoro _      = False

-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
-- lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn dirs   (Fin cofre)                   = error "No existe la posicion"
hayTesoroEn []     mapa                          = hayTesoro mapa
hayTesoroEn (d:ds) (Bifurcacion cofre map1 map2) = 
    if esIzq d
        then hayTesoroEn ds map1
        else hayTesoroEn ds map2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre)                   = []
caminoAlTesoro (Bifurcacion cofre map1 map2) = 
    if hayTesoro map1
    then Izq : caminoAlTesoro map1
    else Der : caminoAlTesoro map2


singularSi x True  = x
singularSi _ False = []

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

--4
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin cofre)                   = []
caminoDeLaRamaMasLarga (Bifurcacion cofre map1 map2) = 
    elegirRamaMasLarga
        (Izq : caminoDeLaRamaMasLarga map1)
        (Der : caminoDeLaRamaMasLarga map2)

elegirRamaMasLarga :: [a] -> [a] -> [a]
elegirRamaMasLarga lvs1 lvs2 = if length lvs1 > length lvs2
                                    then lvs1
                                    else lvs2     

-- Devuelve los tesoros separados por nivel en el árbol.
-- 5. 
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre)                   = []
tesorosPorNivel (Bifurcacion cofre map1 map2) = 
    obtenerTesorosDe cofre :
    juntarNiveles
        (tesorosPorNivel map1)
        (tesorosPorNivel map2)

obtenerTesorosDe :: Cofre -> [Objeto]
obtenerTesorosDe (Cofre objetos) = filtrarTesorosDe objetos

filtrarTesorosDe :: [Objeto] -> [Objeto]
filtrarTesorosDe [] = []
filtrarTesorosDe (x:xs) = 
    if esTesoro x
        then x : filtrarTesorosDe xs
        else filtrarTesorosDe xs

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss       = yss
juntarNiveles xss      []        = xss
juntarNiveles (xs:xss) (ys: yss) = (xs ++ ys) : juntarNiveles xss yss        

-- Devuelve todos lo caminos en el mapa.
-- 6. 
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cofre)           = []
todosLosCaminos (Bifurcacion cofre map1 map2) = [] : consACada Izq (todosLosCaminos map1) ++ consACada Der (todosLosCaminos map2)



------------------- 3. Nave Espacial ---------------------

-- modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
-- dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
-- es la siguiente:

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

--Implementar las siguientes funciones utilizando recursión estructural:
--1. 
-- Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N sector) = sectoresDe sector

sectoresDe :: Tree Sector -> [SectorId]
sectoresDe EmptyT          = []
sectoresDe (NodeT x t1 t2) = obtenerIdDelSector x : sectoresDe t1 ++ sectoresDe t2

obtenerIdDelSector :: Sector -> String
obtenerIdDelSector (S id _ _) = id

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
-- 2. 
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sector) = sumarPoderDeMotores sector

sumarPoderDeMotores :: Tree Sector -> Int
sumarPoderDeMotores EmptyT          = 0
sumarPoderDeMotores (NodeT x t1 t2) = obtenerPoderDeMotoresDe x + sumarPoderDeMotores t1 + sumarPoderDeMotores t2

obtenerPoderDeMotoresDe :: Sector -> Int
obtenerPoderDeMotoresDe (S _ cs _) = sumarPoderDeMotoresDeComponentes cs

sumarPoderDeMotoresDeComponentes :: [Componente] -> Int
sumarPoderDeMotoresDeComponentes []     = 0
sumarPoderDeMotoresDeComponentes (x:xs) = obtenerPoderSiComponenteEsMotor x + sumarPoderDeMotoresDeComponentes xs

obtenerPoderSiComponenteEsMotor :: Componente -> Int
obtenerPoderSiComponenteEsMotor (Motor poder) = poder
obtenerPoderSiComponenteEsMotor _             = 0

-- Propósito: Devuelve todos los barriles de la nave.
-- 3. 
barriles :: Nave -> [Barril]
barriles (N sector) = barrilesDeSector sector

barrilesDeSector :: Tree Sector -> [Barril]
barrilesDeSector EmptyT          = []
barrilesDeSector (NodeT x t1 t2) = obtenerBarrilesDe x ++ barrilesDeSector t1 ++ barrilesDeSector t2

obtenerBarrilesDe :: Sector -> [Barril]
obtenerBarrilesDe (S _ cs _) = obtenerBarrilesDeComponentes cs

obtenerBarrilesDeComponentes :: [Componente] -> [Barril]
obtenerBarrilesDeComponentes []     = []
obtenerBarrilesDeComponentes (x:xs) = obtenerBarrilSiComponenteEsBarril x ++ obtenerBarrilesDeComponentes xs

obtenerBarrilSiComponenteEsBarril :: Componente -> [Barril]
obtenerBarrilSiComponenteEsBarril (Almacen bs) = bs
obtenerBarrilSiComponenteEsBarril _ = []

-- Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
-- 4. 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N sector) = N (agregarSectoresASectorConId cs id sector)

agregarSectoresASectorConId :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarSectoresASectorConId cs id EmptyT          = EmptyT
agregarSectoresASectorConId cs id (NodeT x t1 t2) = (NodeT (agregarComponentesSiSectorEsID x id cs) (agregarSectoresASectorConId cs id t1) (agregarSectoresASectorConId cs id t2))

agregarComponentesSiSectorEsID :: Sector -> SectorId -> [Componente] -> Sector
agregarComponentesSiSectorEsID (S sectorId cs ts) idABuscar csAAgregar = 
    if esSector sectorId idABuscar 
        then (S sectorId (cs ++ csAAgregar) ts)
        else (S sectorId cs ts)

esSector :: String  -> String -> Bool 
esSector id idABuscar = id == idABuscar

-- 5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.


-- 6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.