

-- 

data Alumne = Alu String Double Double Double deriving (Eq,Show)

-- calificar::[Alumne] -> [String]
-- calificar [] = []
-- calificar (primer alumne de la lista : resta d'alumnes)      -- la resta no es data perque no l'has d'utilitzar 
-- calificar ((Alu nom nota1 nota2 nota3):resta)=                  -- que los parentesis no sigui lo que nos faci fallar -> Alu es nom del constructor
  --   if((nota1>4)&&
   --  (nota2>4)&&
    -- (nota3>4)&&
    --- ((nota1*0.2)+(nota2*0.3)+(nota3*0.5))>5)                  
    -- then
     --   (nom ++ " -> Aprovat")
    -- else
      --  (nom ++ " -> Suspes"):calificar resta
                                    

calificar :: [Alumne] -> [String]
calificar [] = []
calificar ((Alu nom n1 n2 n3):rest) = (if (n1>= 4 && n2>=4 && n3>=4)
then (if ((n1*2/10)+(n2*3/10)+(n3*5/10))>=5 then (nom++"->aprovat")
else (nom++"->suspes"))
else nom++"->suspes"):calificar rest


cal::[Alumne]->[String]
cal [] = []
cal ((Alu n a b c):rest) =
        (if(a < 4)
        then "->aprovat"
        else "->suspes"):cal rest