-- 1) a_
sucesor :: Int -> Int 
sucesor n = n + 1

-- b_
sumar :: Int -> Int -> Int 
sumar n m = n + m

-- c_
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- d_
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y 
    then x
    else y

-- 2) 1)
data Dir = Norte | Sur | Este | Oeste

--a_
--Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Sur = Norte
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Oeste = Este

--b) 
-- Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Sur Sur     = True
iguales Norte Norte = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

--c) 
{- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
total o parcial? ¿Por qué? -}
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste

{- 
    ¿Posee una precondición esta función? si, la precondicion es que el argumento Oeste no tiene siguiente direccion
    ¿Es una función total o parcial? ¿Por qué? Es parcial, ya que no responde a todos los posibles argumentos
-}

-- 2) 2) 
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

{-a) 
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. -}
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia = Lunes
ultimoDia = Domingo
{-b) 
Dado un dia de la semana indica si comienza con la letra M. -}
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True 
empiezaConM Miercoles = True
empiezaConM _ = False 

{-c) 
Dado dos dias de semana, indica si el primero viene después que el segundo. -}
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x y = convertirDiaANumero x > convertirDiaANumero y

convertirDiaANumero :: DiaDeSemana -> Int 
convertirDiaANumero Lunes     = 1
convertirDiaANumero Martes    = 2
convertirDiaANumero Miercoles = 3
convertirDiaANumero Jueves    = 4
convertirDiaANumero Viernes   = 5
convertirDiaANumero Sabado    = 6
convertirDiaANumero Domingo   = 7

{-d) 
Dado un dia de la semana indica si no es ni el primer ni el ultimo dia-}
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio x = 
    (convertirDiaANumero x > convertirDiaANumero primerDia) &&
    (convertirDiaANumero x < convertirDiaANumero ultimoDia)

--3)
{-a) 
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not. -}
negar :: Bool -> Bool
negar True = False 
negar False = True

{-b) 
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True. 
Nota: no viene implementada en Haskell. -}
implica :: Bool -> Bool -> Bool
implica True False = False 
implica _ _        = True 

{-c) 
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
En Haskell ya está definida como &&. -}
and :: Bool -> Bool -> Bool
and True True = True 
and _     _   = False

{-d) 
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
En Haskell ya está definida como ||. -}
or :: Bool -> Bool -> Bool
or False b = b
or b False = b
or _ _     = True

-- 3. Registros

data Persona = ConsP String Int

        --1)
--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (ConsP n e) = n

--Devuelve la edad de una persona
edad :: Persona -> Int
edad (ConsP n e) = e

--Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (ConsP n e) = ConsP n (e+1)

--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre newName (ConsP _ e) = ConsP newName e

--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (ConsP n1 e1) (ConsP n2 e2) = e1 > e2

--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = 
    if esMayorQueLaOtra x y
        then x
        else y

-- 2.
data Pokemon = ConsPoke TipoDePokemon Int
data TipoDePokemon = Agua | Planta | Fuego
data Entrenador = ConsEn String Pokemon Pokemon

{- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso. -}
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPoke t1 _) (ConsPoke t2 _) = esTipoSuperior t1 t2

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool 
esTipoSuperior Agua Fuego = True 
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (ConsEn _ pk1 pk2) = unoSi (pokemonEsDeTipo tp pk1) + unoSi (pokemonEsDeTipo tp pk2)

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

pokemonEsDeTipo :: TipoDePokemon -> Pokemon -> Bool
pokemonEsDeTipo tp (ConsPoke tdp _) = esMismoTipoDePokemon tp tdp

esMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool 
esMismoTipoDePokemon Agua Agua = True
esMismoTipoDePokemon Fuego Fuego = True
esMismoTipoDePokemon Planta Planta = True
esMismoTipoDePokemon _ _ = False

-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (x,y) = extraerPokemonesEntrenador x ++  extraerPokemonesEntrenador y

extraerPokemonesEntrenador :: Entrenador -> [Pokemon]
extraerPokemonesEntrenador (ConsEn _ pk1 pk2) = pk1 : [pk2]

--4. Funciones polimórficas
{-Dado un elemento de algún tipo devuelve ese mismo elemento.
a) -} 
loMismo :: a -> a
loMismo x = x 

{-b) 
Dado un elemento de algún tipo devuelve el número 7.-}
siempreSiete :: a -> Int
siempreSiete x = 7

-- c) 
-- Dadas una tupla, invierte sus componentes.
-- ¿Por qué existen dos variables de tipo diferentes? para poder descriminarlas
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

--2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
-- Son polimorficas porque no operan aritmeticamente sobre los parametros

-- 5. Pattern matching sobre listas

--2. 
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
-- Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True 
estaVacia _ = False 

-- 3. 
-- Dada una lista devuelve su primer elemento.
-- Definida en Haskell como head.
-- Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
elPrimero (x:xs) = x

-- 4. 
-- Dada una lista devuelve esa lista menos el primer elemento.
-- Definida en Haskell como tail.
-- Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

-- 5. 
-- Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
-- lista, y la segunda componente es esa lista pero sin el primero.
-- Nota: tener en cuenta que el constructor de listas es :
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)
