























-- Implementar una funcion de alto nivel llamada filter, que acepte dos parametros , el primero sera una funcion con un parametro polimorfico que devolvera un valor booleano, el segundo sera una lista polimorfica. La funcion filter devolvera una lista formada por los elementos de la lista original cuyos elementos cumplan el predicado establecido por la funcion.

--Ejemplo: filter (islessthan 3) [1,7,2,9,67,3];

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' cond (front:rest) = if (cond front) then (front : (filter' cond rest)) else filter' cond rest
















































---------------------------------------

-- Implementar la funcion ZipWith
--zipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
--suma x y = x+y
--Exemple: zipWith suma [1,2,3] [4,5,6]
--[5,7,9]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' func (x:xs) (y:ys) = (func x y) : (zipWith' func xs ys)

























---------------------------------------


-- Construir una funcion 'divide' que tome una lista 'xs' y una condicion 'f' y devuelve (ys,zs) donde 'ys' = primeros elementos que cumplen la condicion y 'zs' = resto de elementos.
--Ejemplo: divide (<3) [1..5]
--([1,2],[3,4,5]).

divide_aux :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a], [a])
divide_aux _ [] ys zs = (ys, zs)
divide_aux f (front:rest) ys zs = divide_aux f rest (if (f front) then (ys ++ [front]) else ys) (if (f front) then zs else (zs ++ [front]))

divide :: (a -> Bool) -> [a] -> ([a], [a])
divide f lista = divide_aux f lista [] []

























---------------------------------------

--Implementar la funcion "foldr_filter" que calcule una operacion para los elementos de una lista que cumplan una condicion especifica.
-- foldr_filter tiene la forma:  <foldr_filter  condicion  operacion valor_inicial  lista>

-- ej. foldr_filter (>4) (+) 0 [3,6,4,5] --> 11      
-- sumaria el 5 y el 6

foldr_filter :: (a -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldr_filter _ _ b [] = b
foldr_filter cond func resultat (front:rest) = foldr_filter cond func (if (cond front) then (func front resultat) else resultat) rest

