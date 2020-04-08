

-- importante

-- Crea la funcion "cuenta_car"
        --  que tenga como parametros 
                -- un caracter y 
                -- una lista de strings 
        --  y devuelve el 
                -- numero de veces que aparece ese caracter en los strings que empiezan por ese caracter. 
                -- Todos los strings estan en minusculas. Hacedlo usando el MAP, FILTER y FOLDR 

-- ej . Main> cuenta_car 'e' ["el","examen","esta","especialmente","escogido","entre","los","mas",""]
-- 15 :: Integer


-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- 

cuentaCar :: Char -> [[Char]] -> Int
cuentaCar car llista =  foldr (+) 0 (map (cuenta car) (filter (parolaCar car) llista))  

cuenta :: Char -> [Char] -> Int
cuenta c [] = 0
cuenta c (front:rest) = if(c == front) 
        then 1+(cuenta c rest) 
        else cuenta c rest
 
parolaCar :: Char -> [Char] -> Bool
parolaCar car (x:xs) = (x == car)



-- origen esquivar palabras -> filter

-- primero tratar palabras -> map

-- segundo sumar todos ->  foldr


 

--cuentaCar :: Char -> [[Char]] -> Int
--cuentaCar x list = foldr (+) 0 (map (cuentaCarAux x ) (filter (cuentaCarEmpieza x) list))
-- 

--cuentaCarEmpieza :: Char -> [Char] -> Bool
--cuentaCarEmpieza x (front:rest) = if(front == x) then True else False 

--cuentaCarAux :: Char -> [Char] -> Int
--cuentaCarAux x (front:rest) = if(x == front) then 1 + cuentaCarAux x rest else cuentaCarAux x rest  
--cuentaCarAux x [] = 0 
 
-------------------------------------------------

-- Crea la funcion "quita_primos" 
        -- que tenga como parametros 
                -- un numero y 
                -- una lista de enteros y 
        -- nos devuelva el resultado de 
        -- restar al numero todos los numeros primos que aparezcan. 
-- Hacedlo usando el MAP, FILTER y FOLDR y la funcion "factorizar" de la siguiente forma:
-- Pista:  Los numeros primos son aquellos cuya factorizacion tiene longitud 1 

-- ej. Main> quita_primos 100 [2,3,4,5,6,7,8,9,10]
--           83 :: Integer


-- map 
-- filter
-- foldr
-- factorizar

-- es primo implica length 1

factorizar :: Int -> [Int]
factorizar n = factorizarAux 2 n

factorizarAux :: Int -> Int -> [Int]
factorizarAux index 1 = []
factorizarAux index n = if (n `mod` index == 0)
        then (index : (factorizarAux (index) (n`div` index)))
        else factorizarAux (index+1) n

primo :: Int -> Bool
primo n = length (factorizar n) == 1

quitaPrimo :: Int -> [Int] -> Int
quitaPrimo x lista = x - (foldr (+) 0 (filter primo lista))  

-- 0n aplicar map a los valores ->
-- 1r descartar los maps con un filter
-- 2n sumar total de filtrados
-- 4t resta final




-------------------------------------------------


-- Usando las funciones map,foldr y filter cread la funcion 
        -- "invierte_palabras" que a partir de un texto devuelve
         --  un nuevo texto con todas las palabras invertidas en su centro.

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

invierteCentro :: [[Char]] -> [Char]
invierteCentro x =  foldr (++) "" (map (++" ") (map invierte_centro x)) 

-- map
-- foldr


















-------------------------------------------------


-- Usando el map, filter y foldr, crear la 
        -- funcion "opera_si" que tiene por parametros
                -- una operacion, 
                -- valor inicial para la operacion, 
                -- una condicion dependiente de 2 variables, 
                -- un numero inicial para la comparacion y 
                -- una lista de numeros binarios.
                
-- Esta funcion opera todos los numeros binarios de la lista si cumplen la condicion

-- ej. Main> opera_si (+) 0 (>) 3 ["10","11","111","1110"]
operaSi :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Bool) -> Int -> [[Char]] -> [Char]

-- suma los numeros mayores que 3
-- "10101" :: [Char]                                   
-- "111" + "1110" = "10101" (7+14=21)

-- ej. Main> opera_si (*) 1 (\x y -> (mod x y) == 0) 2 ["10","11","111","1110"] 
-- multiplica los pares
-- "11100" :: [Char]                                                       
-- "10" * "1110" = "11100" (2*14=28)

operaSi opera ini cond numini lista = binario (foldr opera ini (filter (\x -> cond x numini) (map decimal' lista)))

-- 0 map pasar de strinbin a decimal
-- 1r filter los que son mayor que 3
-- foldre sumar reultados i retorna binario


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











