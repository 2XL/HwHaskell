-- Escribir una funcion recursiva de pila que sume todos los numeros menores de 3 que aparezcan en una lista de enteros

sumaMenores3 :: [Int] -> Int
sumaMenores3 [] = 0
sumaMenores3 (front:rest) = if (front<3) then (front+(sumaMenores3 rest)) else (sumaMenores3 rest)

------------------------------------------------

-- Escribir una funcion recursiva acumulativa que calcule el maximo elemento de la lista de enteros.

maximoAux :: Int -> [Int] -> Int
maximoAux max [] = max
maximoAux max (front:rest) = maximoAux (if front>max then front else max) rest

maximo :: [Int] -> Int
maximo lista = maximoAux (-999999) lista


------------------------------------------------


-- Crear una funcion listar_enteros (n,lista) que devuelva una lista con los enteros comprendidos entre 0 y n.

listar_enteros :: Int -> [Int] -> [Int]
listar_enteros _ [] = []
listar_enteros n (front:rest) = if (front>0 && front<n) then (front : (listar_enteros n rest)) else (listar_enteros n rest)


------------------------------------------------


-- Crear una funcion listar_entre(inicio,fin,listaX) que devuelva una lista formada por los enteros entre inicio y fin de la lista listaX.

listar_entre :: Int -> Int -> [Int] -> [Int]
listar_entre _ _ [] = []
listar_entre ini fin (front:rest) = if (front>ini && front<fin) then (front : (listar_entre ini fin rest)) else (listar_entre ini fin rest)


------------------------------------------------


-- Modificar el ejercicio 4 para que solo muestre los multiplos de 2.


listar_entre2 :: Int -> Int -> [Int] -> [Int]
listar_entre2 _ _ [] = []
listar_entre2 ini fin (front:rest) = if (front>ini && front<fin && (front `mod` 2 == 0)) then (front : (listar_entre2 ini fin rest)) else (listar_entre2 ini fin rest)


------------------------------------------------


-- Implementar la función "digitos" que recibe por parámetro un entero y lo convierte en una lista de digitos. Hacedlo por recursividad de pila y acumulativa.
-- ej. digitos 3645 --> [3,6,4,5]

digitos :: Int -> [Int]
digitos 0 = []
digitos n = digitos (n `div` 10) ++ [(n `mod` 10)]







digitosAux :: [Int] -> Int -> [Int]
digitosAux lista 0 = lista
digitosAux lista n = digitosAux ((n `mod` 10) : lista) (n `div` 10)

digitos' :: Int -> [Int]
digitos' n = digitosAux [] n


------------------------------------------------


-- Implementa la funcion "suma_div" donde a partir de un numero se obtenga la suma de todos sus divisores sin incluir a si mismo. Hacedlo con recursividad de pila y acumulativa


suma_div_aux :: Int -> Int -> Int
suma_div_aux 0 n = 0
suma_div_aux div n = if (n `mod` div == 0) then (div + (suma_div_aux (div-1) n)) else (suma_div_aux (div-1) n)

suma_div :: Int -> Int
suma_div n = suma_div_aux (n-1) n










suma_div_aux' :: Int -> Int -> Int -> Int
suma_div_aux' suma 0 n = suma
suma_div_aux' suma div n = suma_div_aux' (if (n `mod` div == 0) then (div + suma) else suma) (div-1) n

suma_div' :: Int -> Int
suma_div' n = suma_div_aux' 0 (n-1) n
------------------------------------------------


-- Implementar la función "potencia" que recibe por parámetro un numero entero y el numero al que lo queremos elevar y nos devuelve el resultado de aplicar la operacion. 
-- Se ha de hacer por recursividad de pila.

-- ej.   Main> potencia 3 4
--             81 :: Integer


potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n y = n * potencia n (y-1)

------------------------------------------------


-- Implementar la función "central" que recibe por parámetro una lista y devuelve una estructura con tres elementos: 
-- El primero es el elemento inicial de la lista.
-- El segundo es una lista con los elementos de la lista menores que el elemento inicial.
-- El tercero es una lista con los elementos mayores que el elemento inicial de la lista.
-- Usad la recursividad acumulativa. Los elementos han de conservar el orden inicial.

-- ej.   Main> central "362741"
--             ('3',"21","674") :: (Char,[Char],[Char])

central_aux :: Char -> [Char] -> [Char] -> [Char] -> (Char, [Char], [Char])
central_aux ini menors majors [] = (ini, menors, majors)
central_aux ini menors majors (front:rest) = central_aux ini (if front<ini then (menors++[front]) else menors) (if front>ini then (majors++[front]) else majors) rest

central :: [Char] -> (Char , [Char], [Char])
central (front:rest) = central_aux front [] [] rest


------------------------------------------------

-- Implementar la función "hexadecimal" que recibe por parámetro un entero y lo convierte en su correspondiente numero hexadecimal (numero en base 16 en forma de string). 
-- Podeis usar la funcion 'hexa x', que a partir de un numero decimal te devuelve el caracter de su correspondiente numero hexadecimal.
-- Se ha de hacer por recursividad acumulativa. 

hexa x 
  | x ==0 = '0'
  | x ==1 = '1'
  | x ==2 = '2'
  | x ==3 = '3'
  | x ==4 = '4'
  | x ==5 = '5'
  | x ==6 = '6'
  | x ==7 = '7'
  | x ==8 = '8'
  | x ==9 = '9'
  | x == 10 = 'a'
  | x == 11 = 'b'
  | x == 12 = 'c'
  | x == 13 = 'd'
  | x == 14 = 'e'
  | x == 15 = 'f'

-- ej. Main > hexadecimal 200
--            "c8" :: [Char]
-- ej. Main > hexadecimal 0
--            "0" :: [Char]


hexadecimal_aux :: [Char] -> Int ->  [Char]
hexadecimal_aux res 0 = res
hexadecimal_aux res n = hexadecimal_aux ((hexa (n `mod` 16)) : res) (n `div` 16)

hexadecimal :: Int -> [Char]
hexadecimal 0 = [hexa 0]
hexadecimal n = hexadecimal_aux [] n

------------------------------------------------


-- Implementar la función "decimal" que recibe por parámetro String en forma de numero hexadecimal y devuelve un integer con su correspondiente numero decimal. 
-- Podeis usar la funcion 'deci x', que a partir de un caracter te devuelve su numero decimal asociado.
-- Se ha de hacer por recursividad de pila.

deci x 
  | x =='0' = 0
  | x =='1' = 1
  | x =='2' = 2
  | x =='3' = 3
  | x =='4' = 4
  | x =='5' = 5
  | x =='6' = 6
  | x =='7' = 7
  | x =='8' = 8
  | x =='9' = 9
  | x == 'a' = 10
  | x == 'b' = 11
  | x == 'c' = 12
  | x == 'd' = 13
  | x == 'e' = 14
  | x == 'f' = 15

-- PISTA:  Podeis usar la funcion mirror para cambiar el orden de los caracteres en el String

mirror "" = ""
mirror (front: rest) = (mirror rest) ++ [front]

-- ej. Main > decimal "3a2"
--            930 :: Integer

decimal_aux :: [Char] -> Int -> Int
decimal_aux [] _ = 0
decimal_aux (front:rest) index = ((potencia 16 index) * (deci front)) + decimal_aux rest (index+1)

decimal :: [Char] -> Int
decimal n = decimal_aux (mirror n) 0

------------------------------------------------

-- Implementar la función "primo" que recibe por parámetro un entero y devuelve un
-- booleano que indica si es o no es un numero primo 

-- ej. Main> primo 127
--     True :: Bool

primo_aux :: Int -> Int -> Bool
primo_aux 1 _ = False
primo_aux index n = (n `mod` index == 0) || primo_aux (index-1) n

primo :: Int -> Bool
primo n = not (primo_aux (n-1) n)













------------------------------------------------

-- Implementar la funcion "factorizar" que a partir de un numero entero nos devuelva una lista con su factorizacion ordenada (la lista de numeros primos minimos en los que se puede descomponer). 

-- ej. Main> factorizar 8
--     [2,2,2] :: [Integer]
-- ej. Main> factorizar 100
--     [2,2,5,5] :: [Integer]


factorizar_aux :: Int -> Int -> [Int]
factorizar_aux index 1 = []
factorizar_aux index n = 
        if (n `mod` index == 0) 
        then (index : (factorizar_aux (index) (n `div` index))) 
        else factorizar_aux (index+1) n

factorizar :: Int -> [Int]
factorizar n = factorizar_aux 2 n

------------------------------------------------


-- Implementar la función "expand_pot_2" que recibe por parámetro una lista de enteros con al menos un elemento y en caso de que la longitud de la lista no sea igual a una potencia de 2 (0,1,2,4,8,16,...) la expande colocando 0s hasta que cumpla la condicion de tener la longitud como potencia de 2.
-- Podeis usar la funcion "length" del sistema.(2.0 puntos)

-- ej. Main> expand_pot_2 [1,2,3,4]
--          [1,2,3,4] :: [Integer]
-- ej. Main> expand_pot_2 [1,2,3,4,5]
--          [1,2,3,4,5,0,0,0] :: [Integer]

esPotenciaDeDos :: Int -> Bool
esPotenciaDeDos 2 = True
esPotenciaDeDos num = (num `mod` 2 == 0) && (esPotenciaDeDos (num `div` 2))

expand_pot_2 :: [Int] -> [Int]
expand_pot_2 llista = if (esPotenciaDeDos(length llista)) then (llista) else (expand_pot_2(llista++[0]))



------------------------------------------------


-- Implementar la función "ordena" que recibe por parámetro una lista strings y nos devuelve la lista con todos los elementos ordenados alfabeticamente. 

-- ej. Main> orden_alf ["disfruta","de","este","dia","hasta","que","un","examen","te","lo","estropee"]
--           ["de","dia","disfruta","este","estropee","examen","hasta","lo","que","te","un"] :: [[Char]]

orden_aux :: [Char] -> [[Char]] -> [[Char]]
orden_aux n [] = [n]
orden_aux n (x:xs) = if (n<x) then (n:x:xs) else (x: (orden_aux n xs))

orden_alf :: [[Char]] -> [[Char]]
orden_alf [] = []
orden_alf (x:xs) = orden_aux x (orden_alf xs)


------------------------------------------------


-- Crea las funciones "fact_pila" y "fact_acum" que a partir de un entero nos devuelvan el resultado de aplicarle la funcion factorial. 
-- La primera lo solucionara por recursividad a pila y y la segunda por acumulativa. 
                   
-- ej. Main> fact_pila 4
--     24 :: Integer
-- ej. Main> fact_acum 6
--     720 :: Integer

fact_pila :: Int -> Int
fact_pila 1 = 1
fact_pila n = n * fact_pila (n-1)

fact_acum_aux :: Int -> Int -> Int
fact_acum_aux res 1 = res
fact_acum_aux res n = fact_acum_aux (n*res) (n-1)

fact_acum :: Int -> Int
fact_acum n = fact_acum_aux 1 n


------------------------------------------------


-- Crea la funcion "invierte_centro" que a partir de un string devuelva un nuevo string tal que la primera y la ultima letra se mantengan en su posicion y el resto esten invertidas. 

-- ej. Main> invierte_centro "haskell"
--     "hleksal" :: [Char]   
-- ej. Main> invierte_centro "examen"
--     "eemaxn" :: [Char]             


invierte_centro_aux:: [Char] -> Char -> Char -> [Char] -> [Char]
invierte_centro_aux [] primer ultim mig = primer:mig++[ultim]
invierte_centro_aux (front:rest) primer ultim mig = 
        invierte_centro_aux rest primer 
                (if (rest==[]) 
                        then front 
                                else ultim) 
                (if (rest/=[]) 
                        then (front:mig) 
                                else mig)

invierte_centro::[Char] -> [Char]
invierte_centro (front:rest) = invierte_centro_aux rest front front []

------------------------------------------------


-- Implementar la función "binario" que recibe por parámetro un entero y lo convierte en su correspondiente numero binario (numero en base 2 en forma de string). 
-- Podeis usar la funciones 'char x', que a partir de un numero devuelve su caracter.
-- Se ha de hacer por recursividad acumulativa.

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

-- ej. Main> binario 12
--     "1100" :: [Char]

binario_aux :: [Char] -> Int -> [Char]
binario_aux res 0 = res
binario_aux res n = binario_aux ((char (n `mod` 2)) : res) (n `div` 2)

binario :: Int -> [Char]
binario n = binario_aux [] n

------------------------------------------------


-- Implementar la función "decimal" que recibe por parámetro String en forma de numero binario y devuelve un integer con su correspondiente numero decimal. 
-- Podeis usar la funcion 'int x', que a partir de un caracter te devuelve su numero asociado.
-- Se ha de hacer por recursividad a pila. 

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

-- ej. Main> decimal "0"
--     0 :: Integer    
-- ej. Main> decimal "101011"
--     43 :: Integer         

decimal_aux' :: Int -> [Char] -> Int
decimal_aux' _ [] = 0
decimal_aux' index (front:rest) = ((potencia 2 index) * (int front)) + decimal_aux' (index+1) rest

decimal' :: [Char] -> Int
decimal' n = decimal_aux' 0 (mirror n)

------------------------------------------------


-- Implementar la función "suma_pot2" que recibe por parámetro un numero y nos devuelve el mismo numero en forma de potencias de 2.
-- Para simplificar, supondremos que el valor de la maxima potencia es 9. 

-- ej. Main> suma_pot2 0
--     "0" :: [Char]    
-- ej. Main> suma_pot2 9    
--     "2^3 + 2^0" :: [Char]
-- ej. Main> suma_pot2 13         
--     "2^3 + 2^2 + 2^0" :: [Char]

dos_a_la 0 = 1
dos_a_la num = 2* (dos_a_la (num-1))


suma_pot_2_aux:: Int -> Int -> [Char]
suma_pot_2_aux num (-1) = []
suma_pot_2_aux num pot= 
        if (num>(dos_a_la pot))         
        then ("2^"++(show pot)++" + "++(suma_pot_2_aux (num-(dos_a_la pot)) (pot-1))) 
        else (
                if num==(dos_a_la pot) 
                then ("2^"++(show pot)) 
                else (suma_pot_2_aux num (pot-1)))

suma_pot_2 num = suma_pot_2_aux num 9


------------------------------------------------

-- Implementar la función "sig_primo" que recibe por parámetro un entero y nos devuelve el siguiente numero primo mayor.
-- Dara lo mismo que el numero introducido por parametro sea primo o no, el resultado siempre sera el siguiente, nunca el mismo numero 

-- ej. Main> sig_primo 127
--     131 :: Integer

sig_aux :: Int -> Int
sig_aux n = if (primo n) then n else (sig_aux (n+1))

sig_primo :: Int -> Int
sig_primo n = sig_aux (n+1)


------------------------------------------------


-- Implementa la funcion "ord_letras" donde a partir de un string devuelva otro string con las letras ordenadas alfabeticamente, eliminando los blancos. 

-- ej. Main> ord_letras "el algoritmo de ordenacion en haskell es muy importante"
-- "aaaacddeeeeeeeghiiikllllmmmnnnnoooooprrrsstttuy" :: [Char]

orden_aux' :: Char -> [Char] -> [Char]
orden_aux' n [] = [n]
orden_aux' n (x:xs) = 
              if (n /= ' ') 
              then (
                if (n<x) 
                then (n:x:xs) 
                else (x: (orden_aux' n xs))) 
              else (x:xs)

ord_letras :: [Char] -> [Char]
ord_letras [] = []
ord_letras (x:xs) = orden_aux' x (ord_letras xs)









