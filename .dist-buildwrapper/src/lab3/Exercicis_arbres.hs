-- Escribir una funcion que calcule el maximo elemento de un arbol.

------------------------------

-- Escribir una funcion que nos indique si un elemento pertenece a un arbol.


------------------------------


-- Escribir una funcion que calcule la suma de los elementos de un arbol.


------------------------------


-- Escribir una funcion que calcule la altura de un arbol.

------------------------------

-- a) Implementa la funcion "hijos_mayores" que devuelve un booleano que indica que para todo un arbol numerico, el valor de los hijos es mayor que el valor de sus padres. 

-- b) Implementar la funcion "hacer_mayores" donde pasando un arbol se modifique el valor de los hijos que sean menores que sus padres directos con el valor del padre +1

-- ej:     4                          4
--       /   \                      /   \
--      6     3     pasa a ser:    6     5
--     / \   / \                  / \   / \
--    8   4 9   5                8   7 9   6

-- Usar la definicion de arbol:

data Arbol a = Planta ((Arbol a),a, (Arbol a)) | Anil deriving (Eq)
instance Show a => Show(Arbol a) where
  show Anil = "o"
  show (Planta(a,b,c))= "<" ++ show a ++ "|" ++ show b ++ "|" ++ show c ++ ">"


------------------------------


-- a) Implementa la funcion "arbol_condi" que nos dice cuantos nodos de un arbol cumplen una determinada condicion booleana
-- b) Implementa la funcion "poda" que a partir de una condicion elimina los subarboles de los nodos que cumplan la condicion  
-- c) Usa parametrizacion parcial para que pode todos los nodos multiples de 3 

-- ej: arbol_condi (>5) (Planta(Planta(Anil,7,Anil),4,Planta(Anil,3,Anil))) --> 1
-- ej: poda (>5) (Planta(Planta(Anil,7,Anil),4,Planta(Anil,3,Anil))) --> <o|4|<o|3|o>>
-- ej: poda_multi3 (Planta(Planta(Anil,7,Anil),4,Planta(Anil,3,Anil))) --> <<o|7|o>|4|o>


------------------------------

-- Crea la funcion "mayoria_condi" donde pasandole una condicion nos indica si al menos la midad de los elementos cumplen con la condicion.
-- Podeis usar la funcion "arbol_condi" del ejercicio anterior

-- ej. Main> mayoria_condi (>5) (Planta(Planta(Planta(Anil,3,Anil),6,Planta(Anil,2,Anil)),4,(Planta(Anil,9,Anil))))
--           False :: Bool


------------------------------


-- Implementa la funcion "mayoria_multiplos_3" que nos indique si la mayoria de nodos de un arbol son multiplos de 3. Haced esto parametrizando parcialmente la funcion del ejercicio anterior.

-- ej.  Main> arbol_condi (>5) (Planta(Planta((Planta(Anil,3,Anil),6,(Planta(Anil,2,Anil)),4,(Planta(Anil,9,Anil))))
-- 2 :: Integer

------------------------------


-- Implementar la función "profundidad" que a partir de un arbol devuelve un entero que marca la profundidad maxima del arbol.

-- ej.  Main> profundidad (Planta(Planta(Planta(Planta(Anil,6,Anil),4,Anil),2,Anil),1,Planta(Anil,3,Planta(Anil,5,Anil))))
-- 4 :: Integer

------------------------------


-- Implementa la funcion "arbol_primo" que devuelve un booleano que indica que para todo un arbol numerico los nodos que aparecen son numeros primos.
--   Podeis usar la funcion "primo" 

-- ej. Main> arbol_primo (Planta (Planta (Anil,3,Anil),7,Planta (Anil,5,Anil)))
--           True :: Bool
-- ej. Main> arbol_primo (Planta (Planta (Anil,4,Anil),8,Planta (Anil,5,Anil)))
--           False :: Bool


------------------------------


-- Implementa la funcion "haz_arbol_primo" que a partir de un arbol numerico transforme los numeros no-primos de los nodos en el numero primo inmediatamente superior.
--  Podeis usar la funcion "primo"

-- ej. Main> haz_arbol_primo (Planta (Planta (Anil,4,Anil),8,Planta (Anil,5,Anil)))
--           <<o|5|o>|11|<o|5|o>> :: Arbol Integer


------------------------------


-- Implementar la función "crea_arboles" que recibe por parámetro una lista de enteros y devuelve una lista de arboles de enteros de un solo elemento (ambos hijos a Anil)

-- ej. Main> crea_arboles [1,2,3]
--           [<o|1|o>,<o|2|o>,<o|3|o>] :: [Arbol Integer]

------------------------------


-- Implementar la función "suma_arboles" que recibe por parámetro dos arboles y como resultado dara un nuevo arbol suma.
-- Definimos la suma de dos arboles a y b como un nuevo arbol donde el subarbol de la izquierda es a, el subarbol de la derecha es b y el nodo sera la suma de los nodos principales de a y b. 
-- En caso de que alguno de los dos arboles sea el arbol vacio contaremos su valor como 0.
-- En caso de sumar dos arboles vacios el resultado sera el arbol vacio

-- ej. Main> suma_arboles (Planta(Anil,2,Anil)) (Planta(Anil,4,Anil))
--           <<o|2|o>|6|<o|4|o>> :: Arbol Integer
-- ej. Main> suma_arboles (Planta(Planta(Anil,1,Anil),2,Planta(Anil,1,Anil))) (Planta(Anil,4,Anil))
--           <<<o|1|o>|2|<o|1|o>>|6|<o|4|o>> :: Arbol Integer
-- ej. Main> suma_arboles Anil Anil
--           o :: Arbol Integer
-- ej. Main> suma_arboles Anil (Planta(Anil,3,Anil))
--           <o|3|<o|3|o>> :: Arbol Integer


------------------------------


-- Implementar la función "suma_lista_arboles" que recibe por parámetro una lista de arboles y les aplica la suma dos a dos (el primero con el segundo, el tercero con el cuarto, etc...), generando una nueva lista con los resultados. En caso de ser una lista de longitud impar, el ultimo arbol se sumara al arbol vacio Anil.

-- ej. Main> suma_lista_arboles [Planta(Anil,4,Anil),Planta(Anil,5,Anil),Planta(Anil,3,Anil)]
-- [<<o|4|o>|9|<o|5|o>>,<<o|3|o>|3|o>] :: [Arbol Integer]
-- ej. Main> suma_lista_arboles [Planta(Anil,4,Anil),Planta(Anil,5,Anil),Planta(Anil,3,Anil),Planta(Anil,2,Anil)]
--           [<<o|4|o>|9|<o|5|o>>,<<o|3|o>|5|<o|2|o>>] :: [Arbol Integer]


------------------------------

--Implementar la función "crea_arbol_suma" de la siguiente forma:
--     a) A partir de una lista de enteros, expandela hasta su potencia de 2 con "expand_pot_2"
--     b) Usa "crea_arboles" para convertir la lista de enteros en una lista de arboles
--     c) Aplica recursivamente "suma_lista_arboles" hasta que solo quede un arbol en la lista
--     d) Extrae el arbol de la lista y devuelvelo como resultado.

-- ej. Main> crea_arbol_suma [1,2,3]
--     <<<o|1|o>|3|<o|2|o>>|6|<<o|3|o>|3|<o|0|o>>> :: Arbol Integer
-- ej. Main> crea_arbol_suma [1,2,3,4]
--     <<<o|1|o>|3|<o|2|o>>|10|<<o|3|o>|7|<o|4|o>>> :: Arbol Integer


------------------------------


-- Crea la funcion "arbol_fig_ord" donde a partir de una arbol de Figuras nos diga si todos los hijos tienen figuras de mayor area que las figuras de los nodos padres

-- ej. Main> arbol_fig_ord (Planta(Planta(Planta(Anil,Rectangulo(2,4),Anil),Triangulo(3,5),Anil),Circulo 1,Anil))
--     True :: Bool 
-- ej. Main> arbol_fig_ord (Planta(Planta(Planta(Anil,Rectangulo(2,4),Anil),Triangulo(3,5),Anil),Circulo 1,Planta(Anil,Rectangulo(1,3),Anil)))
--     False :: Bool

------------------------------

  
-- Implementa la funcion "arbol_binario" que transforma un arbol de enteros a un arbol de numeros binarios. 

--  ej. Main> arbol_binario (Planta(Planta(Anil,7,Planta(Anil,12,Anil)),3,Planta(Planta(Anil,43,Anil),5,Anil)))
--      <<o|"111"|<o|"1100"|o>>|"11"|<<o|"101011"|o>|"101"|o>> :: Arbol [Char]




-- Implementa la funcion "suma_arboles" donde a partir de dos arboles de enteros nos devuelve el arbol suma resultante de sumar cada nodo de un arbol con el nodo analogo del otro arbol.
-- En caso de no existir un nodo en una posicion de un arbol, consideraremos que su valor es 0. (1.5 puntos)

-- ej         5                 3                       8      
--          /   \             /   \                   /   \    
--         3     6     +     4     2        =        7     8    
--        / \   /           /     /  \              / \   / \  
--       2  4  1           7     2    1            9  4   3 1 

-- ej. Main> suma_arboles (Planta(Planta(Anil,2,Anil),4,Planta(Anil,3,Anil))) (Planta(Planta(Anil,4,Planta(Anil,1,Anil)),5,Anil))
--     <<o|6|<o|1|o>>|9|<o|3|o>> :: Arbol Integer
