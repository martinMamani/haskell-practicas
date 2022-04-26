module Set
 (Set, emptyS, addS, belongs, sizeS, removeS,unionS,setToList)
 where
data Set = S [] n
-- inv : no hay elementos repetidos y el n > 0 donde n es la cantidad de elemento del set.

--Crea un conjunto vacío.
emptyS :: Set a
emptyS (S [] n) = S [] 0
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS e (S [] n) = S (agregarSi e c) n+1 
addS e (S c n) = S (e:c n+1)

agregarSioNo :: 
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool

--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int

--Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
