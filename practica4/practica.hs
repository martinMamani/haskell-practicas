-- 1 - PIZZAS

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa| Queso | Jamon| Aceitunas Int deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8)
 (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Salsa (Capa Queso Prepizza)
pizza5 = Capa Queso (Capa Jamon Prepizza)
pizza6 = Capa Jamon (Capa Jamon Prepizza)
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p)= 1 + cantidadDeCapas p


--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)


-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza  = Prepizza
sacarJamon (Capa ing p) =  if esJamon ing 
                              then sacarJamon p 
                              else Capa ing (sacarJamon p) 

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False


-- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = esQuesoOSalsa ing  &&  tieneSoloSalsaYQueso p

esQuesoOSalsa :: Ingrediente ->Bool
esQuesoOSalsa ing = esQueso ing || esSalsa ing

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False


--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p ) =             if esAceituna ing 
     then Capa ing (duplicarAceitunas p)
     else duplicarAceitunas p

esAceituna :: Ingrediente -> Bool
esAceituna Aceitunas = True
esAceituna _ = False