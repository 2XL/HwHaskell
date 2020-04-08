

j = 1 + i
i = 5

twentyFive :: Int
twentyFive = 12 + 13



quadrat ::  Int -> Int
quadrat n = n * n


doble :: Int -> Int
doble d = d * 2


dodrat :: Int -> Int
dodrat n = quadrat(doble n)

dubSqr2 :: Int -> Int 
dubSqr2 = quadrat . doble

-- avgThree :: Float -> Float -> Float -> Float
avgThree x y z =  (x+y+z)/3


difQuad x y = (x-y)^2

-- recursion

-- factorial by primitive recursion on decreasing num
fac1 :: Int -> Int
fac1 n = if n==1 then 1 else (n * fac1 (n-1))


-- factorial by primitive recursion on list tail
fac2 :: Int -> Int
fac2 n = prodList [1 .. n]
prodList lst = if (length lst) == 1 then head lst else head lst * (prodList (tail lst))

 
toList n =  [ 1 .. n ]



-- els valors negatius calen fer en parentesis

absolut :: Int -> Int
absolut n | n >= 0    = n
      | otherwise = -n


myabs :: Int -> Int
myabs n = if n >= 0 then n else -n

-- pattern matching

pList [] = 0
pList [x] = x
pList (x : xs ) = x * pList xs


-- the thrid item of a list or tuple

third (a,b,c,d) = c
three = third (1,2,3,4)


-- is a sequence a subsequence of another sequence?

-- a' contains in a --> isSubSeq a' a -> true else false

isSubSeq [] _ = True -- sequence in subsequence
isSubSeq _ [] = False -- sequence not in subsequence
isSubSeq lst (x:xs) = (lst == start) || isSubSeq lst xs
        where start = take (length lst) (x:xs)


-- guards

prodLst lst
 | length lst==0 = 0
 | length lst==1 = head lst
 | otherwise     = head lst * prodLst (tail lst)

 

-- list comprehension

-- myList :: [(Int, Int, Int)]
-- myList = [(i,j,i*j)
--                        | i <- [2,4..100],
--                          j <- [3,6..100],
--                          0 == ((i+j) `rem` 7)]
                          
-- quicksort

qsort [] = []
qsort (x:xs) = qsort [y 
                        | y<-xs, 
                                y<=x] 
                                ++ [x] 
                                ++ qsort 
                     [y 
                        | y<- xs, 
                                y>x]


 























































