












--------------------------------

-- Implementar la funcion "lista_aprobados" donde a partir de una lista de alumnos con las notas de los 3 parciales nos devuelva un string con los alumnos que han aprobado la asignatura, separados por '|'.
-- Para aprobar es necesario que en los 3 examenes se saque una nota igual o superior a 4 y que la media este por encima de 5.
-- Hacedlo usando el MAP, FILTER, FOLDR, y utilizando la definición de Alumno:

data Alumno = Alu (String,Double,Double,Double) deriving (Eq,Show)

-- ej. Main> lista_aprobados [Alu("Gandalf",9,10,9.5),Alu("Frodo",6,9,4),Alu("Sam",8,3,9), Alu("Merry",4,7,4.5),Alu("Pippin",4.5,5,4)]
--  "Gandalf | Frodo | Merry" :: [Char]

lista_aprobados_aux :: [Alumno] -> String -> String
lista_aprobados_aux [] resultat = tail resultat
lista_aprobados_aux ((Alu (nom, nota1, nota2, nota3)) : rest) resultat = 
        lista_aprobados_aux rest (
                if (nota1>=4 && nota2>=4 && nota3>=4 && ((nota1+nota2+nota3)/3)>5) 
                then (resultat ++ "|" ++ nom) 
                else resultat)

lista_aprobados :: [Alumno] -> String 
lista_aprobados lista = lista_aprobados_aux lista ""












