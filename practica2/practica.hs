-- RECURSION SOBRE LISTAS .

--1 - sumatoria
sumatoria :: [Int]->Int
sumatoria [] = 0
sumatoria (x:xs)=  x  + sumatoria xs


-- 2 - longitud 

longitud :: [a]->Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 3 - sucesores
sucesores :: [Int]->[Int]
sucesores [] = []
sucesores (x:xs) = sucesor x :  sucesores xs 

-- funcion aux
sucesor :: Int -> Int
sucesor n = n + 1

-- 4 - conjuncion
conjuncion :: [Bool]->Bool
conjuncion [] = True
conjuncion (x:xs) = x  && conjuncion xs

-- 5 - disyuncion 
disyuncion :: [Bool]->Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

-- 6 - aplanar 
aplanar :: [[a]] ->[a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs 

-- 7 - pertenece
pertenece :: Eq a => a ->[a] ->Bool
pertenece e [] = False
pertenece e (x:xs) =  e == x || pertenece e xs

-- 8 - apariciones
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) =  unoSiEsIgual e x + apariciones e xs

--funcion aux
-- usamos el eq cuando igualamos
-- osea usamos el (==)
unoSiEsIgual :: Eq a=> a -> a -> Int
unoSiEsIgual a e = if a == e 
                        then 1
                        else 0

-- 9 - losMenoresA
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] =[]
losMenoresA n (x:xs) =  if x < n 
                            then  x : losMenoresA n xs
                             else losMenoresA n xs

--10 - lasDeLongitudMayorA

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) =  if((longitud x) > n) then  x :  lasDeLongitudMayorA n xs else lasDeLongitudMayorA n xs

-- 11 - agregarAlFinal

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs e = xs ++ [e]

-- 12 - concatenar

concatenar :: [a] -> [a] -> [a]
concatenar l [] = l
concatenar [] l = l
concatenar (l:ls) xs =   l :  concatenar ls xs  

-- 13 - reversa

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs)=  agregarAlFinal (reversa xs ) x

--14 -  zipMaximos

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (l:ls) (x:xs) =  max l x :   zipMaximos ls xs 

-- 15 - elMinimo
elMinimo :: Ord a => [a] -> a
elMinimo [a] = a
elMinimo (x:xs) = if x < elMinimo xs
                     then x 
                     else elMinimo xs 


-- RECURSION SOBRE NUMEROS

-- 1 - factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n =   n * factorial (n - 1)

-- 2 - cuentaRegresiva
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva  0 = []
cuentaRegresiva  n =  n :  cuentaRegresiva (n-1)

-- 3 - repetir
repetirA :: Int -> a -> [a]
repetirA 0 e = []
repetirA n e =  e : repetirA (n-1) e

-- 4 - losPrimeros
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros n [] = []  
losPrimeros n (x:xs)=  x : losPrimeros(n-1) xs 

-- 5 - sinLosPrimeros
sinLosPrimeros :: Int -> [a]-> [a]
sinLosPrimeros 0 ls = ls
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs     


-- REGISTROS

-- 1 - 
type Nombre = String
type Edad = Int

data Persona = Per Nombre Edad deriving Show

carlos = Per "carlos" 34
juan = Per "juan" 38
alex = Per "alex" 24
roberto = Per "roberto" 35
maria = Per "maria" 20
luciano = Per "luciano" 18

-- 1 - 
mayoresA :: Int -> [Persona] ->[Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if edadPerEsMayor n p
                      then p : mayoresA n ps
                      else mayoresA n ps 

edadPerEsMayor :: Int -> Persona -> Bool
edadPerEsMayor n (Per no e) = e > n

-- 2 - 
promedioEdad :: [Persona] -> Int
promedioEdad []     = error "Debe haber al menos una persona"
promedioEdad ps = sumatoria(edadesDePersonas ps) `div` longitud(edadesDePersonas ps)

edadesDePersonas :: [Persona] ->[Int]
edadesDePersonas [] = []
edadesDePersonas (p:ps) = edadDe p  :  edadesDePersonas ps

edadDe :: Persona -> Int
edadDe (Per n e) = e

-- 3 - 
elMasViejo :: [Persona] -> Persona
elMasViejo []     = error "Debe haber al menos una persona"
elMasViejo (p:[]) = p
elMasViejo (p:ps) = if (edadDe (p)) > (edadDe (elMasViejo ps))
                        then p
                        else elMasViejo ps


-- 2 - 
type NombreEntrenador = String
type Energia = Int 

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Energia deriving Show
data Entrenador = ConsEntrenador NombreEntrenador [Pokemon] deriving Show

charmander = ConsPokemon Fuego 50
charmeleon = ConsPokemon Fuego 60
charizard = ConsPokemon Fuego 70

squirtle = ConsPokemon Agua 40
wartortle = ConsPokemon Agua 50
blastoise = ConsPokemon Agua 60

bulbasaur = ConsPokemon Planta 30
ivysaur = ConsPokemon Planta 40
venusaur = ConsPokemon Planta 50


-- 1 - 
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n lp) = cantPokemones lp

cantPokemones :: [Pokemon] -> Int
cantPokemones []       = 0
cantPokemones (pk:pks) =  1 + cantPokemones pks

-- 2 - 
cantidadPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadPokemonesDe t (ConsEntrenador n lp) = cantPokemonesDeTipo t lp

cantPokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDeTipo t [] = 0
cantPokemonesDeTipo t (pk:pks) = if esDeMismoTipo t pk 
                                  then 1 + cantPokemonesDeTipo t pks
                                  else 0 + cantPokemonesDeTipo t pks

esDeMismoTipo :: TipoDePokemon -> Pokemon -> Bool
esDeMismoTipo t (ConsPokemon tp e) = esMismoTipo t tp

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False

-- 3 - 
-- Agua supera a fuego
-- Fuego supera a planta
-- planta supera a agua

losQueganan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueganan t entren1 (ConsEntrenador n lp) = losQuegananA t entren1 lp


losQuegananA :: TipoDePokemon -> Entrenador -> [Pokemon] ->Int
losQuegananA t (ConsEntrenador n lp1) lp2 = cantPokemonesGanadores t lp1 lp2 
-- losQuegananA t (ConsEntrenador n lp1) lp2 = cantPokemonesGanadores (pokemonesDeTipo t lp1) lp2 

-- pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
-- pokemonesDeTipo t []       = []
-- pokemonesDeTipo t (pk:pks) =  if esDeMismoTipo t pk 
--                                  then pk : pokemonesDeTipo t pks
--                                  else pokemonesDeTipo t pks
------------------------------------------------------------------------------
cantPokemonesGanadores :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantPokemonesGanadores t [] lp       = 0
cantPokemonesGanadores t (pk:pks) lp = if esPokemonGanador t pk lp  
                                        then 1 + cantPokemonesGanadores t pks lp
                                        else 0 + cantPokemonesGanadores t pks lp 

esPokemonGanador :: TipoDePokemon -> Pokemon -> [Pokemon] -> Bool
esPokemonGanador t p []       =  False
esPokemonGanador t p (pk:pks) = esGanadorPorTipo p pk && esDeMismoTipo t p || esPokemonGanador t p pks

esGanadorPorTipo :: Pokemon -> Pokemon -> Bool 
esGanadorPorTipo (ConsPokemon t e) p = esGanador t p

esGanador :: TipoDePokemon -> Pokemon -> Bool
esGanador t1 (ConsPokemon t2 e) = leGanaTipo t1 t2

leGanaTipo :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaTipo Agua Fuego = True
leGanaTipo Fuego Planta = True
leGanaTipo Planta Agua = True
leGanaTipo _ _ = False


ash = ConsEntrenador "ash" [charmander,squirtle,bulbasaur,charizard]


bruk = ConsEntrenador "bruk" [squirtle,blastoise,squirtle]


