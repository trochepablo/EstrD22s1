data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
{-
    INV.REP: en (N sectores tripulantes tribulantesRango) se cumple que:
            * si un tripulante esta en el map tambien esta en el MaxHeap, y viceversa
            * cada sector tiene nombres de tripulantes que existen en map tripulantes
-}



-- b) 
construir :: [SectorId] -> Nave
-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S log S) 
construir sectores = N (construirSectoresDeNave ss) emptyM emptyH

-- justificacion: por cada SectorId se realiza una operacion constante (crearS) pero
-- (asoccM) la obsorbe en peor caso por ser O(log K).
construirSectoresDeNave :: [SectorId] -> Map SectorId Sector
-- proposito: agregar a un nuevo map un nuevo sector por cada elemento de la lista
-- costo: O(S log S) siendo S la cantidad de claves en el map y la cantidad de elementos de lista
construirSectoresDeNave []     = emptyM
construirSectoresDeNave (s:ss) = asoccM (crearS s) (construirSectoresDeNave ss)

-- c) 
-- justificar: se realizar una operacion constante (crearT) y dos operaciones de costo
-- O(log T) <<asoccM>>, siendo estas ultimas las de peor caso.
ingresarT :: Nombre -> Rango -> Nave -> Nave
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T) siendo T la cantidad de claves en el map y en la maxHeap
ingresarT nm rg (N sectores tps tpsRango) = 
    let nuevoT = crearT nm rg in 
    N sectores (asoccM nuevoT tps) (asoccM nuevoT tps) 

-- d) 
-- Justificacion: porque en peor caso obtenerSectoresDeEn es de costo O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M) siendo M la cantidad de claves en map de tripulantes
sectoresAsignados nm (N sectores tps tpsRango) = obtenerSectoresDeEn nm tps

-- Justificacion: Porque en peor caso lookupM es de costo O(log M)
-- y absorbe a obtenerSectoresDeT que es de coston O(1)
obtenerSectoresDeEn :: Nombre -> Map Nombre Tripulante -> Set SectorId
-- Propósito: Busca el tripulante con nombre nm en el map y obtiene sus sectores
-- Costo: O(log M) siendo M la cantidad de claves en el map
obtenerSectoresDeEn nm tps = obtenerSectoresDeT (lookupM nm tps)

-- justificacion: porque sectoresT es de costo O(1)
obtenerSectoresDeT :: Tripulante -> Set SectorId
-- Propósito: obtiene los sectores del tripulante tp
-- Costo: O(1) operacion sectoresT
obtenerSectoresDeT tp = sectoresT tp

-- e) 
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector sId (N sectores tps tpsRango) = armarTuplaDeSector sId sectores

armarTuplaDeSector :: SectorId -> Map SectorId Sector -> (Set Nombre, [Componente])
armarTuplaDeSector SectorId sectores = 
    let sector = obtenerSector SectorId sectores in
    (tripulantesS sector, componentesS sector)

obtenerSector ::  SectorId -> Map SectorId Sector -> Sector     
obtenerSector SectorId sectores = lookupM sId sectores

-- f) 
tripulantesN :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
tripulantesN (N sectores tps tpsRango) =
    if isEmpty tpsRango
        then []
        else maxH tpsRango : tripulantesN (N sectores tps (deleteMaxH tpsRango))

-- g) 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector cps sID (N sectores tps tpsRango) = N (agregarComponentesASectorEn cps sID sectores) tps tpsRango

agregarComponentesASectorEn :: [Componente] -> SectorId -> (Map SectorId Sector) -> (Map SectorId Sector)
-- costo: O(C + log S)
agregarComponentesASectorEn cps sID sectores = assocM sID (agregarComponentesA cps (lookupM sID sectores)) sectores

agregarComponentesA :: [Componente] -> Sector -> Sector
-- costo: O(C + log S)
agregarComponentesA []     sector = sector
agregarComponentesA (c:cs) sector = agregarC c (agregarComponentesA cs)

-- h) 
asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + T log T)
asignarASector nm sId (N sectores tps tpsRango) = 
    let tripulante = asignarS sId (lookupM nm tps) -- asignarS O(log S) & assocM O(log T)
        sector     = asginarT nm (lookupM sId sectores) in -- asignarT O(log S) & assocM O(log T)
    N (assocM sId sector sectores) (assocM nm tripulante tps) (actualizarTripulanteEn tpsRango tripulante)

