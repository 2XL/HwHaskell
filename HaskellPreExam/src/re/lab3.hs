--



-- haskell -> guia del laboratori


-- estructura de dades

-- creació de DATA  

-- el nom dels constructors no necesariament han de ser igual que el nom de la clase


data Alumne = Alu String Double Double Double deriving (Eq,Show)
--   Alumne -> Alu, nom, nota,  nota,  nota


data Cotxe = Cotxe String String Int deriving (Show)
--   Cotxe : Cotxe marca  model  portes 


data Point = Point Float Float deriving (Show)
--              p  x     y


-- el nom del tipus es figura i te dos contructors diferents
data Figura = Circle Point Float | Rectangle Point Point deriving (Show)
-- cercle , punt i radi, rectangle, dues cantonades oposades


-- data Figura = Cercle Point Float | Rectangle Point Point deriving (Show)   

-- Show, haskell doesn't know how to display our data type as a string yet. haskell runs Show funtion to the string representacion of our value, and then it prints that out to the terminal.

-- Eq, its used to compare the two instances and see if they are the same.

-- es fa servir deriving(Eq, Show), de manera igual que utilitzavem extends o implements


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  

surface :: Figura -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))


-- Exemple de crida: surface (Circle (Point 0 0) 1)
-- Exemple de crida: surface (Rectangle (Point 0 0) (Point 2 2))


-- :t pi
-- pi :: Floating a => a


calificar :: [Alumne] -> [String]
calificar [] = []
calificar ((Alu nom n1 n2 n3):rest) = (if (n1>= 4 && n2>=4 && n3>=4)
 then (if ((n1*2/10)+(n2*3/10)+(n3*5/10))>=5 then (nom++"->aprovat")
 else (nom++"->suspes"))
 else nom++"->suspes"):calificar rest
 
 
-- Exemple de crida: calificar [(Alu "Juan" 6 5 4), (Alu "Pedro" 4 7 9)]




qualif :: [Alumne] -> [String]
qualif [] = []
qualif (( Alu nom n1 n2 n3 ) : rest ) = (if
        (( n1 >= 4 && n2 >= 4 && n3 >= 4) &&
        (((n1*0.2) + (n2*0.3) + (n3*0.5)) >= 5)) then
                (nom++"->Aprovat") else (nom++"->Suspes")):qualif rest




 



















































































