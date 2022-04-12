-- TIPOS RECURSIVOS SIMPLES

-- 1 - celdas con bolitas

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

-- EJEMPLO 

-- 1 - 
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita color celda )= esDeColor c color + nroBolitas c celda


esDeColor :: Color -> Color -> Int
esDeColor Azul Azul = 1
esDeColor Rojo Rojo = 1
esDeColor _ _ = 0

--2 - 
poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c1 celda = ponerB c1 celda

ponerB :: Color -> Celda -> Celda
ponerB c celda = Bolita c celda

celda0= CeldaVacia
celda1 = Bolita Azul CeldaVacia
celda2 = Bolita Azul (Bolita Azul CeldaVacia)
celda3 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
-- sacar :: Color -> Celda -> Celda
sacar :: Color -> Celda -> Celda
sacar cl CeldaVacia = CeldaVacia
sacar cl (Bolita co celda) = if esDeColor cl co 
                                then celda
                                else Bolita co (sacar cl celda)

-- ponerN :: Int -> Color -> Celda -> Celda



