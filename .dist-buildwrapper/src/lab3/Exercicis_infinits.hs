-- Dado el conjunto de numeros que cumplen las condiciones
--    a)  La suma de sus digitos esta entre 25 y 27
--    b)  No tiene digitos repetidos
--  Se pide que se obtengan los n primeros numeros que cumplan estas condiciones

-- Usad las funciones "numsfrom" y "Select".
-- Tambien podeis usar la funcion "digitos"

numsfrom n = n : numsfrom (n+1)

select :: Int -> [a] -> [a]
select n _ | n <= 0 = []
select _ [] = []
select n (x:xs) = x : select (n-1) xs 

-- ej: select 10 conjunto --> [1789,1798,1879,1897,1978,1987,2689,2698,2789,2798]



---------------------------------------------

--Dos numeros son amigos cuando la suma de sus divisores de un numero sin incluir el mismo es igual a otro numero, cuya suma de sus divisores sin incluirle es igual al numero inicial. Si un numero es amigo de si mismo se dice que es un numero perfecto.

--        a) Se pide que seleccionemos los n primeros numeros perfectos 
--        b) Se pide que seleccionemos los n primeros numeros amigos no perfectos. 

-- Usad las funciones "numsfrom" y "Select".
-- Tambien podeis usar la funcion "suma_div"

---------------------------------------------


-- Implementar la lista infinita "npotencias" donde a partir de un numero nos indique todas sus potencias. Podeis usar la funcion "potencia".
-- Usad Lazy Evaluation para obtener las n primeras potencias

-- ej. Main> select 15 (npotencias 2)
--[1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384] :: [Integer]


---------------------------------------------


-- Dado el conjunto infinito de numeros hexadecimales que contienen al menos una letra se pide que se obtengan los n primeros

-- Usad las funciones "numsfrom" y "Select".
-- Tambien podeis usar la funciones "hexadecimal" y "decimal"

-- ej. Main> select 15 con_letra
-- ["a","b","c","d","e","f","1a","1b","1c","1d","1e","1f","2a","2b","2c"]


---------------------------------------------


-- Dado el conjunto infinito de numeros no primos cuya factorizacion da lugar a dos o mas numeros primos de forma que no se repite ningun numero en su factorizacion, se pide que se obtengan los n primeros

-- Usad las funciones "long", "numsfrom" y "Select".
-- Tambien podeis usar las funciones "primo" y "factorizar"


long:: [a] -> Integer
long ([]) = 0
long (front:rest) = 1 + long rest

-- Main> select 10 conjunto 
--       [6,10,14,15,21,22,26,30,33,34] :: [Integer]


---------------------------------------------


-- Segun su polinomio de Taylor, e se puede calcular como:
-- e = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
-- Se pide:
--   a) obtener "eTaylor" como una lista infinita de numeros correspondiente cada posicion
--      a un sumando de la serie de taylor de e. 
--   b) implementar "suma_n_e" como la suma de los n primeros factores de e. 
--   c) obtener "e" como parametrizacion parcial de la suma de los 100 primeros numeros. 
-- Podeis usar "numsfrom", "select" y las funciones "fact_pila" o "fact_acum"

-- ej. Main> select 5 eTaylor
--     [1.0,1.0,0.5,0.166666666666667,0.0416666666666667] :: [Double]
-- ej. Main> suma_n_e 10         
--     2.71828152557319 :: Double
-- ej. Main> e                   
--     2.71828182845905 :: Double


---------------------------------------------

--Dado el conjunto infinito de numeros binarios que tienen mas unos que ceros

-- Usad las funciones "numsfrom" y "Select".
-- Tambien podeis usar la funciones "binario" y "decimal"

-- ej. Main> select 10 mas_unos
--     ["1","11","101","110","111","1011","1101","1110","1111","10011"] :: [[Char]]


---------------------------------------------


-- Dada la serie de fibonacci:
-- 0,1,1,2,3,5,8,13,...
-- donde el primer elemento es 0, el segundo es 1 y a partir del tercero es la suma de sus anteriores, se pide:

-- a) Implementar la funcion "fibo" con parametro su numero de orden 
-- b) Seleccionar los primeros n numeros de la serie
--    Usad las funciones "numsfrom" y "Select"

-- ej. Main> fibo 8
--     13 :: Integer

-- ej. Main> select 15 fibonacci
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377] :: [Integer]
