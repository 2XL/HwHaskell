




















-- Crea la funcion "cuenta_car" que tenga como parametros un caracter y una lista de strings y devuelve el numero de veces que aparece ese caracter en los strings que empiezan por ese 
-- caracter. Todos los strings estan en minusculas. Hacedlo usando el MAP, FILTER y FOLDR 

-- ej . Main> cuenta_car 'e' ["el","examen","esta","especialmente","escogido","entre","los","mas","elementales"]
-- 15 :: Integer

empiezaPor :: Char -> [Char] -> Bool
empiezaPor c (front:rest) = (front == c)

cuenta :: Char -> [Char] -> Int
cuenta c [] = 0
cuenta c (front:rest) = if (front == c) then (1+ cuenta c rest) else cuenta c rest

cuenta_car :: Char -> [[Char]] -> Int
cuenta_car c llista = foldr (+) 0 (map (cuenta c) (filter (empiezaPor c) llista))














-------------------------------------------------

-- Crea la funcion "quita_primos" que tenga como parametros un numero y una lista de enteros y nos devuelva el resultado de restar al numero todos los numeros primos que aparezcan. 
-- Hacedlo usando el MAP, FILTER y FOLDR de la siguiente forma:
-- Pista:  Los numeros primos son aquellos cuya factorizacion tiene longitud 1 

-- ej. Main> quita_primos 100 [2,3,4,5,6,7,8,9,10]
--           83 :: Integer

factorizar_aux :: Int -> Int -> [Int]
factorizar_aux index 1 = []
factorizar_aux index n = if (n `mod` index == 0) then (index : (factorizar_aux (index) (n `div` index))) else factorizar_aux (index+1) n

factorizar :: Int -> [Int]
factorizar n = factorizar_aux 2 n


primo :: Int -> Bool
primo n = length (factorizar n)== 1


quita_primos :: Int -> [Int] -> Int
quita_primos i llista = i - (foldr (+) 0 (filter primo llista))














-------------------------------------------------


-- Usando las funciones map,foldr y filter cread la funcion "invierte_palabras" que a partir de un texto devuelve un nuevo texto con todas las palabras invertidas en su centro.
-- Usad la funcion "invierte_centro" 

-- ej. Main> invierte_palabras ["este","examen","de","haskell","es","muy","facil"]
--     "etse eemaxn de hleksal es muy fical" :: [Char]

-- ej. Main> invierte_palabras ["dicen","que","si","tenemos","un","texto","con","las","palabras","separadas","y","cambiamos","el","orden","interno","manteniendo","la","primera","y","ultima","letra","podemos","ser","capaces","de","leerlo","entendiendo","el","mensaje"]
-- "decin que si tomenes un ttxeo con las parbalas sadarapes y comaibmas el oedrn inretno mdneinetnao la premira y umitla lrtea pomedos ser cecapas de llreeo edneidnetno el mjasnee" :: [Char]


invierte_centro_aux:: [Char] -> Char -> Char -> [Char] -> [Char]
invierte_centro_aux [] primer ultim mig = primer:mig++[ultim]
invierte_centro_aux (front:rest) primer ultim mig = invierte_centro_aux rest primer (if (rest==[]) then front else ultim) (if (rest/=[]) then (front:mig) else mig)

invierte_centro::[Char] -> [Char]
invierte_centro (front:rest) = invierte_centro_aux rest front front []

invierte_palabras :: [[Char]] -> [Char]
invierte_palabras llista = foldr (++) "" (map (++" ") (map invierte_centro llista))














-------------------------------------------------


-- Usando el map, filter y foldr, crear la funcion "opera_si" que tiene por parametros una operacion, valor inicial para la operacion, una condicion dependiente de 2 variables, un numero inicial para la comparacion y una lista de numeros binarios. Esta funcion opera todos los numeros binarios de la lista si cumplen la condicion

-- ej. Main> opera_si (+) 0 (>) 3 ["10","11","111","1110"]
-- suma los numeros mayores que 3
-- "10101" :: [Char]                                   
-- "111" + "1110" = "10101" (7+14=21)

-- ej. Main> opera_si (*) 1 (\x y -> (mod x y) == 0) 2 ["10","11","111","1110"] 
-- multiplica los pares
-- "11100" :: [Char]                                                       
-- "10" * "1110" = "11100" (2*14=28)

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n y = n * potencia n (y-1)

char 0 = '0'
char 1 = '1'
char 2 = '2'
char 3 = '3'
char 4 = '4'
char 5 = '5'
char 6 = '6'
char 7 = '7'
char 8 = '8'
char 9 = '9'


binario_aux :: [Char] -> Int -> [Char]
binario_aux res 0 = res
binario_aux res n = binario_aux ((char (n `mod` 2)) : res) (n `div` 2)

binario :: Int -> [Char]
binario n = binario_aux [] n


int '0' = 0
int '1' = 1
int '2' = 2
int '3' = 3
int '4' = 4
int '5' = 5
int '6' = 6
int '7' = 7
int '8' = 8
int '9' = 9

decimal_aux' :: Int -> [Char] -> Int
decimal_aux' _ [] = 0
decimal_aux' index (front:rest) = ((potencia 2 index) * (int front)) + decimal_aux' (index+1) rest

decimal' :: [Char] -> Int
decimal' n = decimal_aux' 0 (reverse n)

opera_si :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Bool) -> Int -> [[Char]] -> [Char]
opera_si op n cond n2 llista = binario (foldr op n (filter (\x -> cond x n2) (map decimal' llista)))












