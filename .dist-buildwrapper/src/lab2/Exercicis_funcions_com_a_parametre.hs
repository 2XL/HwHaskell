-- Implementar una funcion de alto nivel llamada filter, que acepte dos parametros , 
        -- el primero sera una funcion con un parametro polimorfico que devolvera un valor booleano, 
        -- el segundo sera una lista polimorfica. 
        -- La funcion filter devolvera una lista formada por los elementos de la lista original cuyos elementos cumplan el predicado establecido por la funcion.

--Ejemplo: filter (islessthan 3) [1,7,2,9,67,3];

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = [] -- lista vacia devuelve vacio
filter' a (x:xs) = if( a x ) then x:(filter' a xs) else  (filter a xs)
  
---------------------------------------

-- Implementar la funcion ZipWith
--zipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
suma x y = x+y
--Exemple: zipWith suma [1,2,3] [4,5,6]
--[5,7,9]

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' a (b:bs) (c:cs) = (a b c):(zipWith' a bs cs)

---------------------------------------

-- Construir una funcion 'divide'
        -- que tome una lista 'xs' 
        -- y una condicion 'f' y 
                -- devuelve (ys,zs) donde 
                        --   'ys' = primeros elementos que cumplen la condicion
                        -- y 'zs' = resto de elementos.
--Ejemplo: divide (<3) [1..5]
--([1,2],[3,4,5]).

divide :: (a  -> Bool) -> [a] -> ([a],[a])
divide func (front:rest) = divideAux func (front:rest) [] []

divideAux :: (a  -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
divideAux _ [] a b = (b,a)
divideAux func (front:rest) y z = divideAux func rest (if(func front) then (y) else (y ++ [front])) (if(func front) then (z ++ [front]) else (z))

---------------------------------------

--Implementar la funcion "foldr_filter" 
-- que calcule una operacion para los elementos de una lista que cumplan una condicion especifica.

-- foldr_filter tiene la forma:  <foldr_filter  condicion  operacion valor_inicial  lista>

-- ej. foldr_filter (>4) (+) 0 [3,6,4,5] --> 11      
-- sumaria el 5 y el 6

foldrFilter :: (a -> Bool) ->  (a -> a -> a) ->  a -> [a] -> a
foldrFilter _ _ ini [] = ini
foldrFilter cond oper ini (top:rest) = (foldrFilter cond oper (if(cond top) then  (oper top ini) else ini) rest)

---------------------------------------

-- Implementa la funcion 
 -- "combina" que a partir de 
        -- una condicion, 
        -- dos operaciones y 
        -- dos listas con el mismo numero de elementos, 
                -- aplique la primera operacion a las parejas de elementos que cumplan la condicion y 
                -- aplique la segunda a las que no la cumplan. 

-- Main> combina (\x y -> x>y) (\x y -> x-y) (\x y -> y-x) [2,4,6,8] [7,5,3,1]
-- [5,1,3,7] :: [Integer]

combina :: (a -> a -> Bool) -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a] -> [a]
combina _ _ _ [] [] = []
combina a b c (x:xs) (y:ys) = (if(a x y) then b x y else c x y) : (combina a b c xs ys) 






