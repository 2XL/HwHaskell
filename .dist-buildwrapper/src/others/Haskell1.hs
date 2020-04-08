 


quadratDe :: Int -> Int
quadratDe x = x * x

suma :: Int -> Int -> Int
suma x y = x + y


esMultipleDe3 :: Int -> Bool
esMultipleDe3 x = if (x `mod` 3 == 0) then True else False




llistaCompra = ["pa", "llet", "ous"]
llistaNombres= [1..20]
llistaLletres= ['a'..'z']
parells= [2,4..20]
llistaDeLlistes = [[2,2],[4,4],[6,6],[8,8]]

llistaLletresSenar= ['a','c'..'z']

-- funcions que reben llistes com a parametre

long :: [a] -> Int
long [] = 0             -- cas de sortida quan parare de iterar? quan la llista es buida
long (front:rest) = 1 + long rest -- 


-- funcio que retorna l'element que ocupa una posició concreta de la llista
getnth :: Int -> [a] -> a
getnth 1 (x:xs) = x
getnth n (x:xs) = getnth (n-1) xs


-- tuplas


mascota = ("Xulina", 4, 1200)   -- (nom, numero de potes, pes en grams)
cotxe = ("Chevrolet", "Kalos", "Gris", 1200, 65) -- (marca, model, color, cilindrada, cavalls)
llibre = ("Don Quijote de la Mancha","Miguel de Cervantes", 1605) -- (títol, autor, any) 
biblioteca = [ llibre, ("Romeo y Julieta", "Shakespeare", 1597), ("Oliver Twist", "Dickens", 1837)] -- LLista de Llibres


printdots x = if (x==0) then "" else '.' : printdots(x-1)

-- de pila <- NORMALMENT SI NO DIUEN RES FEM DE PILA
printdotsP :: Int -> [Char]
printdotsP 0 = []
printdotsP n = '.' : printdots (n-1)

-- recursiva acumulativa :  tipic, una funcio auxiliar i un parametre auxiliar de resultat
printdotsaux :: ([Char],Int) -> [Char]
printdotsaux (x,0) = x
printdotsaux (x,y) = printdotsaux ('.':x, y-1)

printdots' :: Int -> [Char]
printdots' n = printdotsaux ([], n)


--
-- recursivitat de pila
sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (front:rest) = front + sumlist rest

-- recursivitat acumulativa
-- aux
xsum :: [Int] -> Int -> Int
xsum [] total = total
xsum (front:rest) total = xsum rest (front+total)
-- main
sumlist' :: [Int]->Int
sumlist' llista = xsum llista 0








