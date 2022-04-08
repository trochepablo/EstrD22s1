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
contarPokeDeTipo tp (x:xs) = unoSi (pokeEsDeTipo x tp)

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

pokeEsDeTipo :: Pokemon -> TipoDePokemon -> Bool  
pokeEsDeTipo (ConsPokemon tpp _) tp = tipoDePokemonEsDeTipo tp tpp

tipoDePokemonEsDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonEsDeTipo Agua Agua     = True
tipoDePokemonEsDeTipo Fuego Fuego   = True
tipoDePokemonEsDeTipo Planta Planta = True
tipoDePokemonEsDeTipo _ _           = True

-- losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
-- esMaestroPokemon :: Entrenador -> Bool
-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
