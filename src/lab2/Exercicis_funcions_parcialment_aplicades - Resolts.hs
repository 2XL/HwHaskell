









-- Parametriza parcialmente la función potencia para calcular la funcion cubo.

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n y = n * potencia n (y-1)

cubo :: Int -> Int
cubo n = potencia n 2

















---------------------------------------------------


--Probar la funcion map con diferentes funciones: incrementar en uno, multiplicar por 2, ...
--Crear la funcion de alto nivel map_inc que parametrice parcialmente map.

map_inc :: [Int] -> [Int]
map_inc = map (+1)

















---------------------------------------------------


-- Implementa "suma_pares" parametrizando parcialmente foldr_filter para que sume los numeros pares de una lista (empezando siempre desde 0). 

-- ej. suma_pares [2,4,5,6]  --> 12  // sumaria el 2,4 y 6

foldr_filter :: (a -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldr_filter _ _ b [] = b
foldr_filter cond func resultat (front:rest) = foldr_filter cond func (if (cond front) then (func front resultat) else resultat) rest

par :: Int -> Bool
par n = n `mod` 2 == 0

suma_pares :: [Int] -> Int
suma_pares = foldr_filter par (+) 0


















---------------------------------------------------


-- Implementar la función "pon_ceros" que a partir de un numero te devuelve un 1 seguido de tantos ceros como se indique. Realizarlo parametrizando parcialmente la funcion
-- "potencia"

-- ej.   Main> pon_ceros 4
--             10000 :: Integer

pon_ceros :: Int -> Int
pon_ceros = potencia 10

















---------------------------------------------------


-- Implementa la funcion "igual_pos" que a partir de dos listas con el mismo numero de elementos, nos devuelva aquellos numeros que aparecen repetidos en la misma posicion o un 0 en aquellas que sean diferentes.
-- Se ha de hacer parametrizando parcialmente la funcion combina

-- Main> igual_pos [2,3,4,5] [2,4,3,5]
--       [2,0,0,5] :: [Integer]

combina _ _ _ [] _ = []
combina _ _ _ _ [] = []
combina cond f1 f2 (x:rest1) (y:rest2) = (if (cond x y) then (f1 x y) else (f2 x y)) : combina cond f1 f2 rest1 rest2

igual_pos :: [Int] -> [Int] -> [Int]
igual_pos = combina (==) (\x y -> x) (\x y -> 0)

















---------------------------------------------------

-- Crea la funcion "cuenta_a" donde a partir de una lista de strings nos suma la cantidad de 'a' solo teniendo en cuenta las palabras que empiezan por 'a'
-- Hacedlo parametrizando parcialmente la funcion "cuenta_car"

-- ej. Main> cuenta_a ["un","anillo","para","atraerlos","a","todos","y","atarlos","en","las","tinieblas"]
--     6 :: Integer

empiezaPor :: Char -> [Char] -> Bool
empiezaPor c (front:rest) = (front == c)

cuenta :: Char -> [Char] -> Int
cuenta c [] = 0
cuenta c (front:rest) = if (front == c) then (1+ cuenta c rest) else cuenta c rest

cuenta_car :: Char -> [[Char]] -> Int
cuenta_car c llista = foldr (+) 0 (map (cuenta c) (filter (empiezaPor c) llista))

cuenta_a :: [[Char]] -> Int
cuenta_a = cuenta_car 'a'

















---------------------------------------------------


-- Define la funcion "cien_menos_primos" parametrizando parcialmente la funcion "quita_primos" de forma que siempre elimine los primos de 100

-- ej. Main> cien_menos_primos [2,3,4,5,6]
--     90 :: Integer

factorizar_aux :: Int -> Int -> [Int]
factorizar_aux index 1 = []
factorizar_aux index n = if (n `mod` index == 0) then (index : (factorizar_aux (index) (n `div` index))) else factorizar_aux (index+1) n

factorizar :: Int -> [Int]
factorizar n = factorizar_aux 2 n


primo :: Int -> Bool
primo n = length (factorizar n)== 1


quita_primos :: Int -> [Int] -> Int
quita_primos i llista = i - (foldr (+) 0 (filter primo llista))

cien_menos_primos :: [Int] -> Int
cien_menos_primos = quita_primos 100

















