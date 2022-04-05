-- REcursion sobre listas .

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