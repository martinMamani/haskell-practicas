-- TIPOS RECURSIVOS SIMPLES

-- 1 - celdas con bolitas

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

-- EJEMPLO Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))


nroBolitas :: Color -> Celda -> Int



poner :: Color -> Celda -> Celda



sacar :: Color -> Celda -> Celda



ponerN :: Int -> Color -> Celda -> Celda



