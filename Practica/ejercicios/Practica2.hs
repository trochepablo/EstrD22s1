module Practica2 where
--import Practica1


-- 1 Recursión sobre listas --

--1. 
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

--2. 
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = sucesor x : sucesores xs

sucesor :: Int -> Int 
sucesor n = n + 1

--4. 
conjuncion :: [Bool] -> Bool
conjuncion []     = True 
conjuncion (x:xs) = x && conjuncion xs

--5 
disyuncion :: [Bool] -> Bool
disyuncion []     = False 
disyuncion (x:xs) = x || disyuncion xs

-- 6 
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = e == x || pertenece e xs 

--8
apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = 
    if e == x
        then 1 + apariciones e xs
        else apariciones e xs

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = 
    if n < x
        then x : losMenoresA n xs
        else losMenoresA n xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = 
    if longitud x > n
        then [x] ++ lasDeLongitudMayorA n xs
        else lasDeLongitudMayorA n xs

--11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e] 
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

--12 
concatenar :: [a] -> [a] -> [a]
concatenar [] ys     = ys
concatenar (x:xs) ys = x : concatenar xs ys

--13
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = concatenar (reversa xs) [x]

-- reversa :: [a] -> [a]
-- reversa []     = []
-- reversa (x:xs) = reversa xs ++ [x]

--14 
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _          = []
zipMaximos _ []          = []
zipMaximos (x:xs) (y:ys) = 
    if x > y
        then x : zipMaximos xs ys
        else y : zipMaximos xs ys

--15 
elMinimo :: Ord a => [a] -> a
elMinimo []     = error "Debe haber al menos 1 elemento"
elMinimo (x:[]) = x
elMinimo (x:xs) = 
    if x < elMinimo xs
        then x
        else elMinimo xs

-- 2 Recursión sobre números --

--1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2 
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs

-- 3 Registros --
data Persona = ConsP String Int

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA em []     = []
mayoresA em (x:xs) =
    if esPersonaDeEdadMayorA em x
        then x : mayoresA em xs
        else mayoresA em xs

--
esPersonaDeEdadMayorA :: Int -> Persona -> Bool
esPersonaDeEdadMayorA em (ConsP _ ep) = em > ep

promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad xs = div (sumarEdadDePersonas xs) (longitud xs)

sumarEdadDePersonas :: [Persona] -> Int
sumarEdadDePersonas []     = 0
sumarEdadDePersonas (x:xs) = obtenerEdadDe x + sumarEdadDePersonas xs

obtenerEdadDe :: Persona -> Int
obtenerEdadDe (ConsP _ e) = e

--
elMasViejo :: [Persona] -> Persona
elMasViejo []     = error "al menos 1 elemento"
elMasViejo (x:[]) = x
elMasViejo (x:xs) =
    if obtenerEdadDe x > obtenerEdadDe (elMasViejo xs)
        then x
        else elMasViejo xs

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (ConsEntrenador _ ps) = contarPokeDeTipo tp ps

contarPokeDeTipo :: TipoDePokemon -> [Pokemon] -> Int
contarPokeDeTipo tp []     = 0
contarPokeDeTipo tp (x:xs) = unoSi (pokeEsDeTipo x tp) + contarPokeDeTipo tp xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

pokeEsDeTipo :: Pokemon -> TipoDePokemon -> Bool  
pokeEsDeTipo (ConsPokemon tpp _) tp = esMismoTipoDePokemon tp tpp

esMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool 
esMismoTipoDePokemon Agua Agua = True
esMismoTipoDePokemon Fuego Fuego = True
esMismoTipoDePokemon Planta Planta = True
esMismoTipoDePokemon _ _ = False

-- losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp (ConsEntrenador _ ps) (ConsEntrenador _ ps2) = cantidadDePokemonesQueLeGananALosOtros (filtrarPokemonesPorTipo ps tp) ps2

filtrarPokemonesPorTipo :: [Pokemon] -> TipoDePokemon -> [Pokemon]
filtrarPokemonesPorTipo [] tp     = []
filtrarPokemonesPorTipo (x:xs) tp = 
    if pokeEsDeTipo x tp
        then x : filtrarPokemonesPorTipo xs tp
        else filtrarPokemonesPorTipo xs tp

cantidadDePokemonesQueLeGananALosOtros :: [Pokemon] -> [Pokemon] -> Int
cantidadDePokemonesQueLeGananALosOtros [] ps = 0
cantidadDePokemonesQueLeGananALosOtros (x:xs) ps = unoSi (pokemonLeGanaATodosLosPokemones x ps) + cantidadDePokemonesQueLeGananALosOtros xs ps

pokemonLeGanaATodosLosPokemones :: Pokemon -> [Pokemon] -> Bool
pokemonLeGanaATodosLosPokemones p []     = True
pokemonLeGanaATodosLosPokemones p (x:xs) = superaA p x && pokemonLeGanaATodosLosPokemones p xs

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 _) (ConsPokemon t2 _) = esTipoSuperior t1 t2

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool 
esTipoSuperior Agua Fuego = True 
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

tiposDepokemon = [Agua, Fuego, Planta]

-- esMaestroPokemon :: Entrenador -> Bool
-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = tieneTodosLosTipos ps tiposDepokemon

tieneTodosLosTipos :: [Pokemon] -> [TipoDePokemon] -> Bool
tieneTodosLosTipos ps []     = True
tieneTodosLosTipos ps (x:xs) = hayAlMenosUnPokemonDeTipo ps x && tieneTodosLosTipos ps xs

hayAlMenosUnPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
hayAlMenosUnPokemonDeTipo [] tp     = False
hayAlMenosUnPokemonDeTipo (x:xs) tp = pokeEsDeTipo x tp || hayAlMenosUnPokemonDeTipo xs tp

charizard = ConsPokemon Fuego 25
flareon = ConsPokemon Fuego 100

lapras = ConsPokemon Agua 50
vaporeon = ConsPokemon Agua 200

gloom = ConsPokemon Planta 75
trecko = ConsPokemon Planta 300

--Entrenadores para probar
red     = ConsEntrenador "Red"  [charizard,lapras,gloom, vaporeon]
gary    = ConsEntrenador "Gary"[trecko,vaporeon]
misty   = ConsEntrenador "Misty" [lapras,vaporeon]
satoshi = ConsEntrenador "Satoshi" [flareon]
blaine  = ConsEntrenador "Blaine" [flareon, charizard, flareon, flareon, flareon]
maestro = ConsEntrenador "Maestro"[charizard, gloom, lapras]

-- 3. -- 

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinRepetidos (extraerProyectosDeRoles rs)

extraerProyectosDeRoles :: [Rol] -> [Proyecto]
extraerProyectosDeRoles []     = []
extraerProyectosDeRoles (r:rs) = extraerProyectoDeRol r : extraerProyectosDeRoles rs

extraerProyectoDeRol :: Rol -> Proyecto
extraerProyectoDeRol (Developer _ p) = p
extraerProyectoDeRol (Management _ p) = p

sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos []     = []
sinRepetidos (x:xs) = 
    if existeProyectoEn x (sinRepetidos xs)
        then sinRepetidos xs
        else x : sinRepetidos xs

existeProyectoEn :: Proyecto -> [Proyecto] -> Bool
existeProyectoEn _ []     = False
existeProyectoEn e (x:xs) = (extraerNombre e) == (extraerNombre x)  || existeProyectoEn e xs         

extraerNombre :: Proyecto -> String 
extraerNombre (ConsProyecto n) = n

-- 2 

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) pys = contarEmpleadosQuePertenecenAProyectos (filtrarSeniors rs) pys

contarEmpleadosQuePertenecenAProyectos :: [Rol] -> [Proyecto] -> Int 
contarEmpleadosQuePertenecenAProyectos [] pys     = 0
contarEmpleadosQuePertenecenAProyectos (r:rs) pys = 
    unoSi (rolPerteneceAProyectos r pys) + contarEmpleadosQuePertenecenAProyectos rs pys

rolPerteneceAProyectos :: Rol -> [Proyecto] -> Bool 
rolPerteneceAProyectos (Developer _ p)  pys = proyectoEstaEn p pys
rolPerteneceAProyectos (Management _ p) pys = proyectoEstaEn p pys

proyectoEstaEn :: Proyecto -> [Proyecto] -> Bool 
proyectoEstaEn proyectoRol [] = False  
proyectoEstaEn proyectoRol (x:xs) = proyectoEsIgualA x proyectoRol || proyectoEstaEn proyectoRol xs 

proyectoEsIgualA :: Proyecto -> Proyecto -> Bool
proyectoEsIgualA (ConsProyecto proyecto) proyectoRol  = sonMismoProyecto proyecto proyectoRol

sonMismoProyecto :: String -> Proyecto -> Bool 
sonMismoProyecto proyecto (ConsProyecto proyectoRol) = proyecto == proyectoRol

filtrarSeniors :: [Rol] -> [Rol]
filtrarSeniors []     = []
filtrarSeniors (x:xs) = 
    if rolesSenioritySenior x 
        then x : filtrarSeniors xs
        else filtrarSeniors xs

rolesSenioritySenior :: Rol -> Bool
rolesSenioritySenior (Developer sn _)  = esSenior sn
rolesSenioritySenior (Management sn _) = esSenior sn

esSenior :: Seniority -> Bool 
esSenior Senior = True
esSenior _      = False 


--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn pys (ConsEmpresa rs) = contarEmpleadosQuePertenecenAProyectos rs pys

--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
--cantidad de personas involucradas.

-- data Seniority = Junior | SemiSenior | Senior
-- data Proyecto = ConsProyecto String
-- data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
-- data Empresa = ConsEmpresa [Rol]

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa roles) = asignadosPorProyectoDe roles

asignadosPorProyectoDe :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoDe []     = []
asignadosPorProyectoDe (x:xs) = armarTuplaPorProyectoDe x : asignadosPorProyectoDe xs

armarTuplaPorProyectoDe :: Rol -> (Proyecto, Int)
armarTuplaPorProyectoDe (Developer  _ py) = (py, 1)
armarTuplaPorProyectoDe (Management _ py) = (py, 1)