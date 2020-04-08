


-- laboratori haskell



-- passar funcions i condicions com a par�metre

-- funcions / parametre


-- condicions / parametre




-- HASKELL - GUIA DEL LABORATORI 2


-- funcio que apartir d'una condici�, dues operacions i dues llistes que tinguin el mateix nombre d'elements,


combina :: (Int -> Int -> Bool) -> (Int->Int->Int) -> (Int->Int->Int) -> [Int] -> [Int] -> [Int]

combina _    _  _       []      _       = []
combina _    _  _       _       []      = []
combina cond f1 f2      (xf:xr) (yf:yr) = 
        (if(cond xf yf) then (f1 xf yf) else (f2 xf yf)):combina cond f1 f2 xr yr 

-- crida:
        -- les funcions d'operaci� han d'anar en parentesis
        -- combina (>) (+) (*) [5,2,3] [4,5,6]
        -- combina (>) (-) (\x y -> y-x) [2,4,6,8] [7,5,3,1] [5,1,3,7]             // \ means function definition or lamba functions
        -- combina (\x y -> x>y) (\x y -> x-y) (\x y -> y-x) [2,4,6,8] [7,5,3,1]
        
-- aplica la primera operaci� a les parelles que compleixen la condici� i aplica la segona operaci� a les parelles que no la compelixen

-- IMPORTANT, indicar que la _ serveix per indicar que el valor d'un parametre d'entrada en es indiferent



-- �s de les funcions // && // cap�aleres: 9


-- map / aplica una operaci� a cada element d'una llista
-- mapa :: (a->b) -> [a] -> [b] -- acepta com a parametre una funci� i un array d'arguments -> retorna un array

suma :: Int -> Int
suma x = x + 1
-- jamas entendere porque va con tacones para ir a la biblioteca a estudiar
-- tampoco entendere porque lleva diadema, algo mas? xD xiang ets un cabr� dels que no n'hihan

-- filter / donada una llista, retorna nom�s aquells elements que compleixen una determinada condici�

-- filter :: (a -> Bool) -> [a] -> [a]

compareBiger5 :: Int -> Bool
compareBiger5 x = x > 5


-- foldr / combina entre elles els elemnts d'una llista, mitjan�ant una operaci� determinada i comen�ant per l'element neutre especificat.  
-- foldr :: (a -> b -> b) -> b -> [a] -> b 
-- foldr :: (a -> b -> b) -> b -> [a] -> b
 

-- les tres funcions anteriors son molt utils per ajudar-nos a implementar-ne de noves.

-- el seguent exemple es una funci� que aprofita les tres estudiades per tal de, donada una llista de nombre d'usuaris enters positius i negatius, retornar el valor absolut m�s gran:

abs_maxi :: [Int] -> Int
abs_maxi lista = foldr(min)(-8)(map(abs)lista)


-- funci� simple amb aplicaci� completa

multiplica :: Int -> Int -> Int

multiplica x y = x*y

multiplicaX2 :: Int -> Int
multiplicaX2 x = x*2 

-- funcions parcialment aplicades


-- 
multiplicaPer2 :: Int -> Int
multiplicaPer2 = multiplica 2


concatenarLlista :: [[a]] -> [a]
concatenarLlista = foldr (++) []

sumLista :: [Integer] -> Integer
sumLista = foldr (+) 0 















