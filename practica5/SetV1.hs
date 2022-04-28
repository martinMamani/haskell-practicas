module SetV1
 (Set, 
 emptyS, 
 addS, 
 belongs, 
 sizeS, 
 removeS,
 unionS,
 setToList,
 set0,
 set1,
 set2,
 set3,
 set12,
 set123)
 where
type Cant = Int
data Set a = SetV1 [a] Cant deriving Show
-- inv : no hay elementos repetidos y el n > 0 donde n es la cantidad de elemento del set.

set0 = emptyS
set1 = addS 1(emptyS)
set2 = addS 2(emptyS)
set3 = addS 3(emptyS)
set12 = addS 1(addS 2(emptyS))
set123 = addS 1(addS 2(addS 3(emptyS)))

--Crea un conjunto vacío.
emptyS :: Set a
emptyS = SetV1 [] 0
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (SetV1 xs n) = if elem x xs then (SetV1 xs n) else (SetV1 (x:xs) (n+1))
 
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs n (SetV1 xs l) = elem n xs  

--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (SetV1 xs n)= n

--Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
-- removeS a (SetV1 xs n) = if elem a xs then removeS a xs else (SetV1 xs n )

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS = undefined

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList = undefined