import SetV1

-- Cálculo de costos
--O(1)
head' :: [a] -> a
head' (x:xs) = x
--O(n)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
--O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
--O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--O(n^2)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
--O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
--O(n^2)

sinRepetidosA :: Eq a => [a] -> [a]
sinRepetidosA [] = []
sinRepetidosA (x:xs) = if pertenece x xs
then sinRepetidosA xs
else x : sinRepetidosA xs

-- equivalente a (++)
--O(n)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
--O(1)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
--O(n)
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
--O(n)
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
--O(2n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
--O(n^2)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
--O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
then xs
else x : sacar n xs
--O(n)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs in m : ordenar (sacar m xs)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
-- Set (conjunto)

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = [] 
losQuePertenecen (x:xs) s = if belongs x s
    then x : (losQuePertenecen xs s)
    else (losQuePertenecen xs s)
                              
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = undefined
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos = undefined
