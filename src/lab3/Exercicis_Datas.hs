
-- A partir de la estructura Numero que representa un numero en su forma decimal y hexadecimal:

data Numero = Num (Integer, String) deriving (Eq,Show)

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


-- definid la funcion "aplica_si" que, a partir de una lista de Numero aplique una operacion a aquellos que cumplan una condicion

-- ej. Main> aplica_si (>15) (*2) [Num (10,"a"), Num (20,"14"), Num (30,"1e")]
-- Num (40,"28"),Num (60,"3c")]  :: [Numero]

-- Podeis usar la función "hexadecimal"

hexadecimal_aux :: [Char] -> Integer ->  [Char]
hexadecimal_aux res 0 = res
hexadecimal_aux res n = hexadecimal_aux ((hexa (n `mod` 16)) : res) (n `div` 16)

hexadecimal :: Integer -> [Char]
hexadecimal 0 = [hexa 0]
hexadecimal n = hexadecimal_aux [] n


--              cond                    oper            numeros         numero filtrado
aplicaSi :: (Integer -> Bool) -> (Integer -> Integer) -> [Numero] -> [Numero]
aplicaSi _ _ [] = []
aplicaSi cond oper ((Num (enter,hex)):rest)  = if( cond enter ) then (Num((oper enter),(hexadecimal (oper enter))) : aplicaSi cond oper rest) else (aplicaSi cond oper rest)



--------------------------------


-- Crea la funcion "aplica" modificando 
        -- la funcion del ejercicio anterior para que, antes de aplicar las funciones, 
        -- verifique que el numero decimal y el hexadecimal son los mismos. 
        -- En caso de no serlo eliminara el numero de la lista. 

-- ej. Main> aplica (>15) (*2) [Num (10,"a"), Num (20,"14"), Num (30,"fffff")]
--           [Num (40,"28")] :: [Numero]

-- Podeis usar las funciones "hexadecimal" y "decimal"


iguals :: Numero -> Bool
iguals (Num (enter, hex))= (hex == (hexadecimal enter)) 

 
aplica :: (Integer -> Bool) -> (Integer -> Integer) -> [Numero] -> [Numero]
aplica _ _ [] = []
aplica cond oper llista = aplicaSi cond oper (filter iguals llista) 
 

--------------------------------


-- Crea la funcion "cuadrado" que a partir de 
        -- una lista de Numero nos eleve todos los numeros al cuadrado. 
        -- Haced esto parametrizando parcialmente la funcion "aplica" del ejercicio anterior

-- ej. Main> cuadrado [Num (2,"2"), Num (3,"fffff"), Num (4,"4"), Num (5,"5")]
-- [Num (4,"4"),Num (16,"10"),Num (25,"19")] :: [Numero]



cuadrado :: [Numero] -> [Numero]
cuadrado = aplica (\x->True) (\x -> x*x) 


--------------------------------


--El tipo de datos "Figura" 
        -- puede ser 
                -- un circulo, definido por su radio, 
                -- un triangulo, definido por su base y altura o 
                -- un rectangulo tambien definido por su base y por su altura.

data Figura = 
          Circulo (Double) 
        | Triangulo (Double,Double) 
        | Rectangulo (Double,Double) deriving (Eq, Show)

-- Define la funcion "area" que a partir de una figura nos de el area de la misma. 
        -- Podeis usar la constante "pi" definida por el sistema. 

-- ej. Main> area (Circulo 3)
--     28.2743338823081 :: Double
-- ej. Main> area (Rectangulo (2,3))
--     6.0 :: Double
-- ej. Main> area (Triangulo (2,3))
--     3.0 :: Double


area :: Figura -> Double
area (Circulo(radio)) = pi*radio*radio
area (Rectangulo(base, altura)) = base * altura
area (Triangulo(base, altura)) = (base*(altura/2))










--------------------------------


-- Implementar la funcion "lista_aprobados" 
        -- donde a partir de una lista de alumnos con las notas de los 3 parciales 
        -- nos devuelva un string con los alumnos que han aprobado la asignatura,
        --  separados por '|'.
-- Para aprobar es necesario que en los 3 examenes
        -- se saque una nota igual o superior a 4 
        -- y que la media este por encima de 5.
        
-- Hacedlo usando el MAP, FILTER, FOLDR, y utilizando la definición de Alumno:

data Alumno = Alu (String,Double,Double,Double) deriving (Eq,Show)

-- ej. Main> listaAprobados [Alu("Gandalf",9,10,9.5),Alu("Frodo",6,9,4),Alu("Sam",8,3,9), Alu("Merry",4,7,4.5),Alu("Pippin",4.5,5,4)]
--  "Gandalf | Frodo | Merry" :: [Char]


listaAprobados :: [Alumno] -> String
listaAprobados l =  foldr (\x y -> x++" | "++y) [] (map getName (filter aprobado l))

getName :: Alumno -> String 
getName (Alu(nom, n1, n2, n3)) = nom

aprobado :: Alumno -> Bool
aprobado (Alu (nom, n1, n2, n3)) =  if((n1 >= 4) && (n2 >= 4) && (n3 >= 4) && ( ((n1+n2+n3 )/ 3) > 5)) then True else False
-- map -> alums filter

-- filter -> aprobado

-- foldr -> retorn









