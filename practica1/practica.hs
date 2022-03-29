--Dado un número devuelve su sucesor

-- 1-  Numeros enteros

-- 1 - Defina las siguientes funciones

sucesor :: Int -> Int
sucesor n = n + 1

-- Dados dos números devuelve su suma utilizando la operación +.

sumar :: Int -> Int -> Int 
sumar n m = n + m 

-- Dado dos números, devuelve un par donde la primera componente es 
-- la división del
-- primero por el segundo, y la segunda componente es el resto de 
-- dicha división. Nota:
-- para obtener el resto de la división utilizar la función 
-- mod :: Int -> Int -> Int,
-- provista por Haskell.

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (n `div` m , mod n m )



-- Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if n > m then n
                           else m


-- 2 - De 4 ejemplos de expresiones diferentes que denoten el 
-- número 10, utilizando en cada expresión a todas las funciones del punto anterior.
-- Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

-- numero 10 

-- sucesor(sumar 4 5)
-- sucesor(sumar (maxDelPar(divisionYResto 15 5) 6) )
-- sumar(sucesor(divisionYResto 50 10) 4)
-- sumar( maxDelPar(8,4) 2) 

-- 2 - Tipos enumerativos



