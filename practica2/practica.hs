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

