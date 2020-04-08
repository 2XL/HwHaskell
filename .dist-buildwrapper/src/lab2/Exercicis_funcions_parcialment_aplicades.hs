







-- Parametriza parcialmente la función potencia para calcular la funcion cubo.


potencia :: Int -> Int -> Int 
potencia n 0 = 1
potencia n y = n * potencia n (y-1)

cubo :: Int -> Int
cubo n = potencia n 2















---------------------------------------------------


--Probar la funcion map con diferentes funciones: incrementar en uno, multiplicar por 2, ...
--Crear la funcion de alto nivel map_inc que parametrice parcialmente map.


map' :: (a -> a) -> [a] -> [a]
map' func (front:rest) =  (func front) : (map' func rest)


mapInc1' :: [Int] -> [Int]
mapInc1' = map (+1)


mapMult2 :: [Int] -> [Int]
mapMult2 = map (*2)













---------------------------------------------------


-- Implementa "suma_pares" 
 -- parametrizando parcialmente 
        -- foldr_filter para que sume los numeros pares 
        -- de una lista (empezando siempre desde 0). 

-- ej. suma_pares [2,4,5,6]  --> 12  // sumaria el 2,4 y 6

sumaPares :: [Int] -> Int
sumaPares = foldrFilter par (+) 0


par :: Int -> Bool
par n = n `mod` 2 == 0


foldrFilter :: (a -> Bool) ->  (a -> a -> a) ->  a -> [a] -> a
foldrFilter _ _ ini [] = ini
foldrFilter cond oper ini (top:rest) = (foldrFilter cond oper (if(cond top) then  (oper top ini) else ini) rest)
 









---------------------------------------------------


-- Implementar la función "pon_ceros" 
        -- que a partir de un numero te devuelve un 1 seguido de tantos ceros como se indique.
        --  Realizarlo parametrizando parcialmente la funcion

-- "potencia"

-- ej.   Main> pon_ceros 4
--             10000 :: Integer




ponCeros :: Int -> Int
ponCeros = potencia 10














---------------------------------------------------


-- Implementa la funcion "igual_pos" que a partir de 
        -- dos listas con el mismo numero de elementos, nos 
        -- devuelva aquellos numeros que aparecen 
                -- repetidos en la misma posicion 
                -- o un 0 en aquellas que sean diferentes.
-- Se ha de hacer parametrizando parcialmente la funcion 
 -- combina

-- Main> igual_pos [2,3,4,5] [2,4,3,5]
--       [2,0,0,5] :: [Integer]

igualPos :: [Int] -> [Int] -> [Int]
igualPos = combina (==) (\x y-> x) (\x y -> 0)


combina :: (a -> a -> Bool) -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a] -> [a]
combina _ _ _ [] [] = []
combina a b c (x:xs) (y:ys) = (if(a x y) then b x y else c x y) : (combina a b c xs ys) 







---------------------------------------------------

--Crea la funcion "cuenta_a" donde a partir de una lista de strings nos suma la cantidad de 'a' solo teniendo en cuenta las palabras que empiezan por 'a'
-- Hacedlo parametrizando parcialmente la funcion "cuenta_car"

-- ej. Main> cuenta_a ["un","anillo","para","atraerlos","a","todos","y","atarlos","en","las","tinieblas"]
--     6 :: Integer



cuentaA :: [[Char]] -> Int
cuentaA = cuentaCar 'a' 


cuentaCar :: Char -> [[Char]] -> Int
cuentaCar x list = foldr (+) 0 (map (cuentaCarAux x ) (filter (cuentaCarEmpieza x) list))
-- 

cuentaCarEmpieza :: Char -> [Char] -> Bool
cuentaCarEmpieza x (front:rest) = if(front == x) then True else False 

cuentaCarAux :: Char -> [Char] -> Int
cuentaCarAux x (front:rest) = if(x == front) then 1 + cuentaCarAux x rest else cuentaCarAux x rest  
cuentaCarAux x [] = 0 











---------------------------------------------------


-- Define la funcion "cien_menos_primos" parametrizando parcialmente la funcion "quita_primos" de forma que siempre elimine los primos de 100

-- ej. Main> cien_menos_primos [2,3,4,5,6]
--     90 :: Integer


cienMenosPrimos :: [Int] -> Int
cienMenosPrimos = quita_primos 100



factorizar_aux :: Int -> Int -> [Int]
factorizar_aux index 1 = []
factorizar_aux index n = if (n `mod` index == 0) then (index : (factorizar_aux (index) (n `div` index))) else factorizar_aux (index+1) n

factorizar :: Int -> [Int]
factorizar n = factorizar_aux 2 n
 


primo :: Int -> Bool
primo n = length (factorizar n)== 1 -- me facto i soc 1 aleshores soc prime


quita_primos :: Int -> [Int] -> Int
quita_primos i llista = i - (foldr (+) 0 (filter primo llista))









