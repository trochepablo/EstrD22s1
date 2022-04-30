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


tesoro   = Tesoro
chatarra = Chatarra
objetosConUnoDeCada  = [tesoro,chatarra]
objetosConTesoros    = [tesoro,tesoro]
cofreVariado         = (Cofre objetosConUnoDeCada)
cofreSoloTesoros     = (Cofre objetosConTesoros)
cofreVacio           = (Cofre [])
mapa1                = Fin cofreVacio
mapa2                = (Bifurcacion cofreVariado mapa1 mapa1)
mapa3                = (Bifurcacion cofreSoloTesoros mapa1 mapa2)
mapa4                = (Bifurcacion cofreVacio mapa2 mapa3)
mapa5                = (Bifurcacion cofreVacio mapa4 mapa3)

mapaIzqLargo = (Bifurcacion cofreVacio (Fin cofreVacio) mapa5)

    --                                     mapa5
    --                     mapa4                           mapa3
    --         mapa2                 mapa3         mapa1             mapa2
    -- mapa1           mapa1      mapa1   mapa2     

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
caminoAlTesoro (Fin cofre)                   = error "Debe existir un tesoro"
caminoAlTesoro (Bifurcacion cofre map1 map2) = 
    if hayTesoroEnCofre cofre
        then []
        else elegirCaminoAlTesoro map1 map2

elegirCaminoAlTesoro :: Mapa -> Mapa -> [Dir]
elegirCaminoAlTesoro map1 map2 =   
        if hayTesoro map1
            then Izq : caminoAlTesoro map1
            else Der : caminoAlTesoro map2

singularSi x True  = [x]
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
        (caminoDeLaRamaMasLarga map1)
        (caminoDeLaRamaMasLarga map2)

elegirRamaMasLarga :: [Dir] -> [Dir] -> [Dir]
elegirRamaMasLarga lvs1 lvs2 = if length lvs1 > length lvs2
                                    then Izq : lvs1
                                    else Der : lvs2



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
todosLosCaminos (Bifurcacion cofre map1 map2) = consACada Izq (todosLosCaminos map1) ++ consACada Der (todosLosCaminos map2)



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
agregarSectoresASectorConId cs id (NodeT x t1 t2) = NodeT (agregarComponentesSiSectorEsID x id cs) (agregarSectoresASectorConId cs id t1) (agregarSectoresASectorConId cs id t2)

agregarComponentesSiSectorEsID :: Sector -> SectorId -> [Componente] -> Sector
agregarComponentesSiSectorEsID (S sectorId cs ts) idABuscar csAAgregar = 
    if esSector sectorId idABuscar 
        then S sectorId (cs ++ csAAgregar) ts
        else S sectorId cs ts

esSector :: String  -> String -> Bool 
esSector id idABuscar = id == idABuscar


-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
-- 5. 
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tp ids (N sector) = N (asignarTripulantesASectores sector ids tp)

asignarTripulantesASectores :: Tree Sector -> [SectorId] -> Tripulante -> Tree Sector
asignarTripulantesASectores EmptyT ids tp          = EmptyT
asignarTripulantesASectores (NodeT x t1 t2) ids tp = NodeT (agregarTripulanteSiSectorPerteneceA ids x tp) (asignarTripulantesASectores t1 ids tp) (asignarTripulantesASectores t2 ids tp)

agregarTripulanteSiSectorPerteneceA :: [SectorId] -> Sector -> Tripulante -> Sector
agregarTripulanteSiSectorPerteneceA ids sector tp = 
    if perteneceSectorA sector ids
        then agregarTripulanteASector sector tp
        else sector

agregarTripulanteASector :: Sector -> Tripulante -> Sector
agregarTripulanteASector (S idSector cs tps) tp = (S idSector cs (tp:tps))

perteneceSectorA :: Sector -> [SectorId] -> Bool
perteneceSectorA sector []       = False
perteneceSectorA sector (id:ids) =  esMismoSector id sector || perteneceSectorA sector ids

esMismoSector :: String -> Sector -> Bool
esMismoSector id (S idSector _ _ ) = id == idSector

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
-- 6. 
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tp (N sector) = sectoresDeTripulante sector tp

sectoresDeTripulante :: Tree Sector -> Tripulante -> [SectorId]
sectoresDeTripulante EmptyT tp           = []
sectoresDeTripulante (NodeT x t1 t2) tp = 
    if hayTripulanteEn x tp
        then obtenerIdDelSector x : sectoresDeTripulante t1 tp ++ sectoresDeTripulante t2 tp
        else sectoresDeTripulante t1 tp ++ sectoresDeTripulante t2 tp

hayTripulanteEn :: Sector -> Tripulante -> Bool
hayTripulanteEn (S _ _ tps) tp = perteneceTripulanteEn tp tps

perteneceTripulanteEn :: Tripulante -> [Tripulante] -> Bool
perteneceTripulanteEn tp []     = False
perteneceTripulanteEn tp (x:xs) = x == tp || perteneceTripulanteEn tp xs

-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
-- 7. 
tripulantes :: Nave -> [Tripulante]
tripulantes (N sector) = sinTripulantesRepetidos (tripulantesSinRepetidos sector)

tripulantesSinRepetidos :: Tree Sector -> [Tripulante]
tripulantesSinRepetidos EmptyT          = []
tripulantesSinRepetidos (NodeT x t1 t2) = obtenerTripulantes x ++ tripulantesSinRepetidos t1 ++ tripulantesSinRepetidos t2

obtenerTripulantes :: Sector -> [Tripulante]
obtenerTripulantes (S _ _ tps) = tps

sinTripulantesRepetidos :: [Tripulante] -> [Tripulante]
sinTripulantesRepetidos []     = []
sinTripulantesRepetidos (x:xs) = 
    if existeTripulanteEn x (sinTripulantesRepetidos xs)
        then sinTripulantesRepetidos xs
        else x : sinTripulantesRepetidos xs

existeTripulanteEn :: Tripulante -> [Tripulante] -> Bool
existeTripulanteEn _ []     = False
existeTripulanteEn e (x:xs) = e == x || existeTripulanteEn e xs


-- 4. Manada de lobos --
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

-- 1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
-- crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
-- que corresponda en cada caso:
manada1 = M (Cazador "Tommy" [] (Explorador "Billy" [] (Cria "NoName1") (Cria "NoName2")) (Explorador "Sam" [] (Cria "NoName3") (Cria "NoName4")) (Cria "NoName5")) 

-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
-- 2. 

buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = alimentoEn presas
                                                + cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
                                                + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2)      = cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                    = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M lobo) = cantidadDeCriasL lobo

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cazador _ presas l1 l2 l3) = cantidadDeCriasL l1
                                             + cantidadDeCriasL l2
                                             + cantidadDeCriasL l3
cantidadDeCriasL (Explorador _ _ l1 l2)      = cantidadDeCriasL l1
                                             + cantidadDeCriasL l2
cantidadDeCriasL (Cria _)                    = 1


-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
-- cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
-- cero presas.
-- 3. 
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom presas l1 l2 l3) = elegirEntre (nom, alimentoEn presas)
                                                    (elegirEntre (elAlfaL l1)
                                                                 (elegirEntre (elAlfaL l2)
                                                                              (elAlfaL l3)))
elAlfaL (Explorador nom _ l1 l2)      = elegirEntre (elAlfaL l1)
                                                    (elegirEntre (elAlfaL l2)
                                                                 (nom, 0))
elAlfaL (Cria nom)                    = (nom, 0)


elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1>=c2) then (nom1, c1)
                                                else (nom2, c2)


-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
-- pasaron por dicho territorio.
-- 4. 
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron tr (M lobo) = nombresDeLobosExploradoresDeTerritorio tr lobo

nombresDeLobosExploradoresDeTerritorio :: Territorio -> Lobo -> [Nombre]
nombresDeLobosExploradoresDeTerritorio tr (Cria nm)                     = []
nombresDeLobosExploradoresDeTerritorio tr (Cazador _ _ lob1 lob2 lob3)  = nombresDeLobosExploradoresDeTerritorio tr lob1 ++ nombresDeLobosExploradoresDeTerritorio tr lob2 ++ nombresDeLobosExploradoresDeTerritorio tr lob3
nombresDeLobosExploradoresDeTerritorio tr (Explorador nm trs lob1 lob2) = 
    if perteneceTeritorrioA tr trs
        then nm : nombresDeLobosExploradoresDeTerritorio tr lob1 ++ nombresDeLobosExploradoresDeTerritorio tr lob2
        else nombresDeLobosExploradoresDeTerritorio tr lob1 ++ nombresDeLobosExploradoresDeTerritorio tr lob2

perteneceTeritorrioA :: Territorio -> [Territorio] -> Bool
perteneceTeritorrioA tr []     = False
perteneceTeritorrioA tr (x:xs) = tr == x || perteneceTeritorrioA tr xs

-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
-- dicho territorio. Los territorios no deben repetirse.
-- 5. 

-- type Presa = String -- nombre de presa
-- type Territorio = String -- nombre de territorio
-- type Nombre = String -- nombre de lobo
-- data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cría Nombre
-- data Manada = M Lobo


-- manada = (M (Explorador "juan" ["lanus", "boca"] (Cazador "pepe" [] (
--         Explorador "Cacho" ["lanus", "guernica"] (Cria "") (Cria "")
--     ) (
--         Cria ""
--     ) (
--         Explorador "flaco" ["boca", "mg", "city"] (Cria "") (Cria "")
--     )) (Cria "criaza"))
manada =  (M (Explorador "juan" ["lanus", "boca"] (Cria "") (Explorador "flaco" ["boca", "mg", "city"] (Cria "") (Cria ""))))

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = procesarTerritoriosDeLobo lobo

procesarTerritoriosDeLobo :: Lobo -> [(Territorio, [Nombre])]
procesarTerritoriosDeLobo (Cria _)                      = []
procesarTerritoriosDeLobo (Cazador _ _ lob1 lob2 lob3)  = 
    agregarATerritoriosSinRepetidos(
            agregarATerritoriosSinRepetidos 
                (procesarTerritoriosDeLobo lob1)
                (procesarTerritoriosDeLobo lob2)
        )
        (procesarTerritoriosDeLobo lob3)
procesarTerritoriosDeLobo (Explorador nm trs lob1 lob2) = 
    agregarATerritoriosSinRepetidos
            (agregarNombreATerritoriosExplorados nm trs (procesarTerritoriosDeLobo lob1))
            (procesarTerritoriosDeLobo lob2)

agregarNombreATerritoriosExplorados :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarNombreATerritoriosExplorados nm []     trs = []
agregarNombreATerritoriosExplorados nm (x:xs) trs = agregarNombreATerritorioExplorado nm x trs ++ agregarNombreATerritoriosExplorados nm xs trs 


agregarNombreATerritorioExplorado :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarNombreATerritorioExplorado nm tr []            = [(tr, [nm])]
agregarNombreATerritorioExplorado nm tr ((t,nms):trs) = 
    if t == tr
        then (t, nm:nms):trs
        else agregarNombreATerritorioExplorado nm tr trs

agregarATerritoriosSinRepetidos :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarATerritoriosSinRepetidos []            trs2 = []
agregarATerritoriosSinRepetidos ((t,nms):trs) trs2 = agregarATerritorioSinRepetidos nms t trs2 ++ agregarATerritoriosSinRepetidos trs trs2

agregarATerritorioSinRepetidos :: [Nombre] -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarATerritorioSinRepetidos nms tr []              = [(tr, nms)]
agregarATerritorioSinRepetidos nms tr ((t, nms2):trs) =
    if tr == t
        then (t, nms++nms2):trs
        else agregarATerritorioSinRepetidos nms tr trs

-- 6. superioresDelCazador :: Nombre -> Manada -> [Nombre]
-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
-- cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
