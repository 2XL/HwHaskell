-- Escribir una funcion recursiva de pila que sume todos los numeros menores de 
-- 3 que aparezcan en una lista de enteros

-- ex1

llistaEnteros :: [Int]
llistaEnteros = [1,5,6,9,8,7,4,5,2,3,6,4,2,5,6,4,5,100,-50,10]

sumaMenor3 :: [Int] -> Int
sumaMenor3 [] = 0
sumaMenor3 (x:xs) = 
        if(x > 3)
                then (x+ (sumaMenor3 xs))
                else sumaMenor3 xs
                          
------------------------------------------------

-- Escribir una funcion recursiva acumulativa que calcule el maximo elemento de la lista de enteros.


maximoLista :: [Int] -> Int
maximoLista x = maximoListaAux x (-100) 
 
maximoListaAux :: [Int] -> Int -> Int
maximoListaAux [] max = max -- si es llista buida el maxim soc jo
maximoListaAux (x:xs) max = if(x > max)
        then maximoListaAux xs x
        else maximoListaAux xs max

------------------------------------------------

-- Crear una funcion listar_enteros (n,lista) que devuelva una lista con los enteros comprendidos entre 0 y n.

listarEnteros :: Int -> [Int] -> [Int]
listarEnteros n [] = []
listarEnteros n (x:xs) = if(x < n && x > 0)
        then (x:(listarEnteros n xs))
        else listarEnteros n xs

------------------------------------------------

-- Crear una funcion listar_entre(inicio,fin,listaX) que devuelva una lista formada por los enteros entre inicio y fin de la lista listaX.

llistarEntre :: Int -> Int -> [Int] -> [Int]
llistarEntre ini 0 list = []
llistarEntre ini fin (x:xs) = if(ini<=0) 
        then x:(llistarEntre (ini-1) (fin-1) xs)
        else llistarEntre (ini-1) (fin-1) xs
        
 
------------------------------------------------


-- Modificar el ejercicio 4 para que solo muestre los multiplos de 2.
llistarEntre' :: Int -> Int -> [Int] -> [Int]
llistarEntre' ini 0 list = []
llistarEntre' ini fin (x:xs) = if((ini<=0)&&(x`mod`2 == 0)) 
        then x:(llistarEntre' (ini-1) (fin-1) xs)
        else llistarEntre' (ini-1) (fin-1) xs
        

 
------------------------------------------------


-- Implementar la función "digitos" que recibe por parámetro un entero y lo convierte en una lista de digitos. Hacedlo por recursividad de pila y acumulativa.
-- ej. digitos 3645 --> [3,6,4,5]
 


llistaEnteros' :: [Int]
llistaEnteros' = [1,5,6,9,8,7,4,5,2,3,6,4,2,5,6,4,5,100,-50,10]



-- de pila

digitos :: Int -> [Int]
digitos 0 = []
digitos x  = digitos (x`div`10) ++ [(x`mod`10)] 
        


-- acumulativa

digitos' :: Int -> [Int]
digitos' x = digitosAux x [] -- [] valor incial array null


digitosAux :: Int -> [Int] -> [Int]
        -- caso de salida 
digitosAux 0 list = list 
        -- caso recursivo 
digitosAux x list = digitosAux (x`div`10) ((x`mod`10) : list)

 
------------------------------------------------

-- Implementa la funcion "suma_div" donde a partir de un numero se obtenga la suma de todos sus divisores sin incluir a si mismo. Hacedlo con recursividad de pila y acumulativa

-- pila

sumaDivAux :: Int -> Int -> Int
sumaDivAux x 0 = 0
sumaDivAux x y = if(x`mod`y== 0) 
        then y+(sumaDivAux x (y-1))
        else sumaDivAux x (y-1) 

sumaDiv :: Int -> Int
sumaDiv x = sumaDivAux x (x-1) 
        


-- acumulativa

sumaDiv' :: Int -> Int
sumaDiv' x = sumaDivAux' x (x-1) 0



--             input - index - acum
sumaDivAux' :: Int -> Int -> Int -> Int -- suma div aux sempre té una variable més que suma div de pila aux
sumaDivAux' x 0 z = z
sumaDivAux' x y z = 
        if(x `mod` y == 0)
                then sumaDivAux' x (y-1) (z+y)
                else sumaDivAux' x (y-1) (z)

------------------------------------------------

-- Implementar la función "potencia" que recibe por parámetro un numero entero y el numero al que lo queremos elevar y nos devuelve el resultado de aplicar la operacion. 
-- Se ha de hacer por recursividad de pila.

-- ej.   Main> potencia 3 4
--             81 :: Integer

-- pila
potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x y = x * potencia x (y-1) 

------------------------------------------------

-- Implementar la función "central" que recibe por parámetro una lista y devuelve una estructura con tres elementos: 
-- El primero es el elemento inicial de la lista.
-- El segundo es una lista con los elementos de la lista menores que el elemento inicial.
-- El tercero es una lista con los elementos mayores que el elemento inicial de la lista.
-- Usad la recursividad acumulativa. Los elementos han de conservar el orden inicial.

-- ej.   Main> central "362741"
--             ('3',"21","674") :: (Char,[Char],[Char])

 
-- acumulativa

central :: [Char] -> (Char, [Char], [Char])
central (x:xs) = centralAux x [] [] xs 

centralAux :: Char -> [Char] -> [Char] -> [Char] -> (Char, [Char], [Char]) 
centralAux x y z [] = (x, y, z)
centralAux x y z (k:ks) = centralAux x  
        (if(x < k) then y++[k] else y) 
       (if(x > k) then z++[k] else z) 
        ks
 
 


------------------------------------------------

-- Implementar la función "hexadecimal" que recibe por parámetro un entero y lo convierte en su correspondiente numero hexadecimal (numero en base 16 en forma de string). 
-- Podeis usar la funcion 'hexa x', que a partir de un numero decimal te devuelve el caracter de su correspondiente numero hexadecimal.
-- Se ha de hacer por recursividad acumulativa. 

-- hexadecimal :: Int -> Char 

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

-- acumulativa

hexadecimal :: Int -> [Char]
hexadecimal 0 = [hexa 0] 
hexadecimal x = hexadecimalAux x []

hexadecimalAux :: Int -> [Char] -> [Char]  
hexadecimalAux 0 ret = ret
hexadecimalAux x ret = hexadecimalAux (x`div`16) (hexa (x`mod`16):ret) 

-- ej. Main > hexadecimal 0
--            "0" :: [Char]

-- ej. Main > hexadecimal 200
--            "c8" :: [Char]

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

-- de pila

mirror :: [Char] -> [Char]
mirror "" = ""
mirror (x:xs) = mirror (xs) ++ [x]

decimal :: [Char] -> Int
decimal x = decimalAux (mirror x) 0


decimalAux :: [Char] -> Int -> Int
decimalAux [] _ = 0
decimalAux (x:xs) y = ((potencia 16 y) * (deci x)) + decimalAux xs (y+1)
 
-- ej. Main > decimal "3a2"
--            930 :: Integer

 

------------------------------------------------

-- Implementar la función "primo" que recibe por parámetro un entero y devuelve un
-- booleano que indica si es o no es un numero primo 

-- ej. Main> primo 127
--     True :: Bool
 
primo :: Int -> Bool
primo x = not (primoAux x (x-1)) 

primoAux :: Int -> Int -> Bool
primoAux _ 1 = False
primoAux x y = (x`mod`y == 0) || primoAux x (y-1) 
 
------------------------------------------------
 
-- Implementar la funcion "factorizar" que a partir de un numero entero nos devuelva 
-- una lista con su factorizacion ordenada 
-- (la lista de numeros primos minimos en los que se puede descomponer). 


-- ej. Main> factorizar 8
--     [2,2,2] :: [Integer]
-- ej. Main> factorizar 100
--     [2,2,5,5] :: [Integer]
 

        
factAux :: Int -> Int -> [Int]
factAux 1 y = []        -- cas de sortida
factAux x y = 
        if(x `mod` y == 0 )  -- s'ha trobat un divisible
                then x : (factAux (x`div`y) y )
                else factAux (x) (y+1)
 
 
factorizar :: Int -> [Int]
factorizar x = factAux x 2
        
 
 
------------------------------------------------

-- Implementar la función "expand_pot_2" que recibe por parámetro una lista de enteros 
-- con al menos un elemento y en caso de que la longitud de la lista no sea igual
-- a una potencia de 2 (0,1,2,4,8,16,...) la expande colocando 0s hasta que cumpla 
-- la condicion de tener la longitud como potencia de 2.

-- Podeis usar la funcion "length" del sistema.(2.0 puntos)



-- cal implementar una función auxiliar





-- ej. Main> expand_pot_2 [1,2,3,4]
--          [1,2,3,4] :: [Integer]
-- ej. Main> expand_pot_2 [1,2,3,4,5]
--          [1,2,3,4,5,0,0,0] :: [Integer]

-- calcular llista length

esPot2 :: Int -> Bool
esPot2 2 = True -- cas de neutre
esPot2 x = (x`mod`2 == 0)&& (esPot2 (x `div`2))

--
expandPot2 :: [Int] -> [Int]
expandPot2 l = 
        if(esPot2 (length l)) then (l) else (expandPot2 (l++[0]))
           
-- entendre molbé lo de concatenar amb  2  ++ en :

-- ++ retorna una llista resultant de concatenar les dues llistes indicades del mateix tipus
-- :  afegeix un element al principi d'ina llsita (no serveix per a dos llistes)

------------------------------------------------
 
-- una lista strings y nos devuelve la lista con todos los elementos ordenados alfabeticamente. 

-- ej. Main> orden_alf ["disfruta","de","este","dia","hasta","que","un","examen","te","lo","estropee"]
--           ["de","dia","disfruta","este","estropee","examen","hasta","lo","que","te","un"] :: [[Char]]


-- com realitzar el lantejament

-- ordenar :: [[Char]] -> [[Char]]
-- ordenar [] = [] 
-- ordenar (x:xs) = ordenarAux x (ordenar xs)

-- ordenarAux :: [Char] -> [[Char]] -> [[Char]]
-- ordenarAux x [] = [x]
-- ordenarAux (x) (y:ys) = 
 --       if(x<y)
   --             then x:y:ys
     --           else (x: (ordenarAux (y) ys))

 
-- comparar :: [Char] -> [Char] -> Bool -- soc mes gran?
-- comparar [] _ = False
-- comparar _ [] = True
-- comparar (x:xs) (y:ys) = if(x<y)
   --     then False
     --   else if(x == y) then comparar xs ys else True

-- ordenar2 x = x > 'c'

ordenarAux :: [Char] -> [[Char]] -> [[Char]]
ordenarAux n [] = [n]
ordenarAux n (x:xs) = if (n<x) then (n:x:xs) else (x:(ordenarAux n xs))

ordenar :: [[Char]] -> [[Char]]
ordenar [] = []
ordenar (x:xs) = ordenarAux x (ordenar xs)

------------------------------------------------

-- Crea las funciones "fact_pila" y "fact_acum" que a partir de un entero nos devuelvan el resultado de aplicarle la funcion factorial. 
-- La primera lo solucionara por recursividad a pila y y la segunda por acumulativa. 
                                                                       
-- ej. Main> fact_pila 4
--     24 :: Integer

factPila :: Int -> Int
factPila 0 = 1
factPila x = x * factPila (x-1)

-- ej. Main> fact_acum 6
--     720 :: Integer
 
factAcum :: Int -> Int 
factAcum x = factAcumAux x 1


factAcumAux :: Int -> Int -> Int
factAcumAux 0 y = y
factAcumAux x y = factAcumAux (x-1) y*x
 
------------------------------------------------

-- Crea la funcion "invierte_centro" que a partir de un string devuelva 
-- un nuevo string tal que la primera y la ultima letra se mantengan en su posicion 
-- y el resto esten invertidas. 

-- ej. Main> invierte_centro "haskell"
--     "hleksal" :: [Char]   
-- ej. Main> invierte_centro "examen"
--     "eemaxn" :: [Char]             
 
 
invertCenter :: [Char] -> [Char]
invertCenter (primer:mig) = primer:invertCenterAux mig []

invertCenterAux :: [Char] -> [Char] -> [Char]
invertCenterAux [] (x:xs) = (xs ++ [x])
invertCenterAux (mig:rest) tupla = invertCenterAux rest ([mig] ++ tupla) 
  
------------------------------------------------

-- Implementar la función "binario" que recibe por parámetro un entero y 
-- lo convierte en su correspondiente numero binario (numero en base 2 en forma de string). 


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

binario :: Int -> [Char] 
binario x = binarioAux x []

binarioAux :: Int -> [Char] -> [Char]
binarioAux 0 retorn = retorn
binarioAux n retorn = binarioAux (n`div`2) ((char (n`mod`2)): retorn)

-- ej. Main> binario 12
--     "1100" :: [Char]

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

-- pila
decimal' :: [Char] -> Int
decimal' n = decimalAux' (mirror n) 0  
--  el 0 es el index inicial caldra fer servir mes metodes implementades 
-- com per exemple la potencia per poder obtenir el resultat proposat

decimalAux' :: [Char] -> Int -> Int -- input ,  index , outputScale
decimalAux' [] _ = 0
decimalAux' (x:xs) index = (potencia 2 index) * (int x) + (decimalAux'  xs (index+1))

------------------------------------------------

-- Implementar la función "suma_pot2" que recibe por parámetro un numero y nos devuelve el mismo numero en forma de potencias de 2.
-- Para simplificar, supondremos que el valor de la maxima potencia es 9. 

-- ej. Main> suma_pot2 0
--     "0" :: [Char]    
-- ej. Main> suma_pot2 9    
--     "2^3 + 2^0" :: [Char]
-- ej. Main> suma_pot2 13         
--     "2^3 + 2^2 + 2^0" :: [Char]

sumaPot2 :: Int -> [Char]
sumaPot2 x = sumaPot2Aux x 9 []

-- seria fer la inversa, tenint com a referencia un maxim de 9

-- recibe args: value, index, scaled 
sumaPot2Aux :: Int -> Int -> [Char] -> [Char]
sumaPot2Aux x (-1) z = z
sumaPot2Aux x y z =  
        if((x - (potencia 2 y)) >= 0) 
        then sumaPot2Aux (x-(potencia 2 y)) (y-1) (z ++ "2^" ++ ([char y]) ++ " ") 
        else sumaPot2Aux x (y-1) z 
 
------------------------------------------------

-- Implementar la función "sig_primo" que recibe por parámetro un entero y nos devuelve el siguiente numero primo mayor.
-- Dara lo mismo que el numero introducido por parametro sea primo o no, el resultado siempre sera el siguiente, nunca el mismo numero 

-- ej. Main> sig_primo 127
--     131 :: Integer

sigPrimo :: Int -> Int
sigPrimo x = sigPrimoAux x x

sigPrimoAux :: Int -> Int -> Int
sigPrimoAux x y = 
        if(y > x) 
        then y
        else 
                if(primo(x+1))
                then sigPrimoAux x (x+1)
                else sigPrimoAux (x+1) y
 
------------------------------------------------


-- Implementa la funcion "ord_letras" donde a partir de un string devuelva otro string con las letras ordenadas alfabeticamente, eliminando los blancos. 

-- ej. Main> ord_letras "el algoritmo de ordenacion en haskell es muy importante"
-- "aaaacddeeeeeeeghiiikllllmmmnnnnoooooprrrsstttuy" :: [Char]


ordLetras :: [Char] -> [Char]
ordLetras [] = []
ordLetras (x:xs) = ordLetrasAux x (ordLetras xs)

ordLetrasAux :: Char -> [Char] -> [Char]
ordLetrasAux n [] = [n]  
ordLetrasAux n (x:xs) = 
        if(n /= ' ')  
        then (  if(n<x)
                then(n:x:xs)
                else(x: (ordLetrasAux n xs)))
        else (x:xs)










