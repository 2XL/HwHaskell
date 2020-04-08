
-- haskell laboratori 1



-- operacions aritmetiques
divisio a b = a / b
--
divent a b = a `div` b
--
modul a b = a `mod` b
--
arodonit a = floor a
--
absolut a = abs a
--


-- operacions logiques
igual a b = a && b
--
alternativa a b = a || b
--
negacio a = not a
-- 
equivalent a b = a == b 
-- 
diferent a b = a /= b


-- altres operacions
succecio a = succ a
--
maxim a b = max a b
--
minim a b = min a b


-- comentaris
        -- per escriure decimals es fan servir punt, no pas la coma
        -- per escriure caracters es fan servir punts simples
        -- per escriure string es fan servir commetes dobles
        
        
        

-- funcions i tasques.
quadratDe x = x * x
suma x y = x + y
esMultipleDe3 x = if(x`mod`3 == 0) then True else False

-- capçaleres de les funcions, previsment a la definició de la funció es pot incloure una capçalera que determina el tipus de les seves entrades i sortides.
-- format capçalera
nomFunc :: String -> String
nomFunc x = x++" es l'argument"
-- el darrer argument es el tipus de retorn


-- llistes
-- les llistes han d'emagatzemar sempre dades del mateix tipus,

llistaCompra = ["pa", "llet", "ous"]
llistaNombres=[1..20]
llistaLletres=['a'..'z']
parells=[2,4..20]
llistaDeLlistes = [[2,2],[4,4],[6,6],[8,8]]
-- lo anterior es lo mateix que una funció lo que varia es que no accepta parametres i sempre retorna el mateix valor

-- tuples
-- les tuples poden contenir tipus parametritzables diferents


-- operacions amb llistes

concatenaLlista a b = a ++ b

afegirElementLlista a b = a : b -- s'afegix el principi es a dir a es el valor a afegir en la llista

elementLlista a b = a !! b -- retorna l'element que es troba en la posició x de la llista

primerElementLlista a = head a
ultimElementLlista a = last a -- retorna el ultim element de la llista

totsMenysPrimer a = tail a -- retorna tots els elements menys el primer

totsMenysUltim a = init a --  retorna tots els elements menys el ultim

logitudLlista a = length a -- retorna la longitud de la llista, retorna el nombre d'elements que té la llista

llistaNula a = null a -- retorna un boolea si la llista es nula

llistaInversa a = reverse a -- reveteix els components de una lista en quant als seus indexos.
 
xPrimersElementsLlista a b = take b a

retornaLlistaExcepteXprimers a b = drop a b
 
maximLlista a = maximum a
minimLlista a = minimum a

sumLlista a = sum a
prodLlista a = product a
llistaConteElem a llista = elem a llista 

-- exemple de funcions que reben llistes com a paràmetres d'entrada.
--
-- primer:resta



longitud :: [a] -> Int
longitud [] = 0 -- cas de retorn
longitud (front:rest) = 1+longitud rest


getElementByIndex :: Int -> [a] -> a -- el tipus de retorn es parametritzat
getElementByIndex 1 (front:rest) = front
getElementByIndex n (front:rest) = getElementByIndex (n-1) rest


-- tuples 
-- s'utilitzen per poder agrupar diversos valors, podent ser de diferents tipus. semblant als objectes java.
-- cada valor equivaldria un atribut.


bilma = ("Gos", 4, 10) -- aclariment: animal, numero de potes, pes
 
mascota = ("Xulina", 4, 1200)                           -- (nom, numero de potes, pes en grams)
cotxe = ("Chevrolet", "Kalos", "Gris", 1200, 65)        --  (marca, model, color, cilindrada, cavalls)
llibre = ("Don Quijote de la Mancha","Miguel de Cervantes", 1605)

-- tipus 
biblioteca = [ llibre, ("Romeo y Julieta", "Shakespeare", 1597), ("Oliver Twist","Dickens", 1837)]


-- afegir un nou llibre a la biblioteca
addBook a = ("Tom Sawyer", "Mark Twain", 1876) : a

-- tipus de recursivitat
-- exemple 1

-- be cool man be cool

printDots x = if(x==0) 
        then ""
        else '.' : printDots(x-1)

-- tamateix es poden expresar la funció de manera més clara de les seguents maneres.



-- RECURSIVITAT DE PILA

printDotPila :: Int -> [Char]
printDotPila 0 = []
printDotPila n = '.' : printDotPila (n-1)



-- RECURSIVITAT ACUMULADA
printDotAcum :: Int -> [Char]
printDotAcum n = printDotAcumAux ([], n)

printDotAcumAux :: ([Char], Int) -> [Char]
printDotAcumAux (x, 0) = x      -- la unicadiferencia es que s'estalvia de passar una llista buida com a element neutre
printDotAcumAux (x, y) = printDotAcumAux('.':x, y-1)

-- en una llista bi -es decir compuesto por dos elementos sabemos que siempres sera compuesta de esta manera, mediante un validador en el contructor para, hacer que se cumpla la condición a cada iteración.




-- R-pila
        -- el cas de sortina retorna l'elemnt neutre de l'operació
        -- el cas recursiu sempre té dos parts diferencials, la primera consisteix en realitzar la feina epr al cas conegut i la segona es la crida a la mateixa funció però passant-li com entrada el problema simplificat, ambdues parts estan relacionades d'un mode o altre segons el problema ( suma, concatenació, etc)
        
        
        
-- R-acumulada
        -- te un parametre extra consistent en la solució parcial del problema
        -- el cas de sortida consisteix en retornar aquest parametre extra
        -- el cas recursiu consisteix en cridar la mateixa funció, canviant els paràmetres., complir i simplificar el problema a cada interació fins en solucionar tots. de forma exhaustiva.
   
        
        
 
-- funcio suma tots els elments d'una llista


-- recursivitat de pila
sumaLlista :: [Int]->Int
sumaLlista [] = 0
sumaLlista (front: rest) = front + sumaLlista rest        
        

-- recursivitat acumulativa
sumaLlista' :: [Int] -> Int
sumaLlista' llista = auxSumaLlista llista 0 -- la lista i l'argument extra per incrementar el sumatori

auxSumaLlista :: [Int] -> Int -> Int
auxSumaLlista [] total = total
auxSumaLlista (front : rest) total = auxSumaLlista rest (front + total)












































































































































































































