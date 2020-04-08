
-- llibreries
import Data.Char (ord)


-- 18/11/2013




-- ********************************************************************** simple intro

-- 1 print hello world

fHelloWorld = putStr "Hello World"



-- 2 functions in haskell

increment :: Int -> Int
increment x = x + 1


and1 :: Bool -> Bool -> Bool
and1 a b = if a == b then a else False


-- and2 :: Bool -> Bool -> Bool
and2 True True = True
-- and2 x y = False  -- Pattern match(es) are overlapped In an equation for `and2': and2 _ _ = ...
and2 _ _ = False -- linea complementaria?


-- 3 calculate roots of quadratic equations

roots (a, b, c) = (x1, x2) where        --roots :: Floating t => (t, t, t) -> (t, t)
        x1 = e + sqrt d / (2*a)
        x2 = e - sqrt d / (2*a)
        d = b*b-4*a*c
        e = -b/ (2*a)

p1,p2 :: (Float, Float, Float)
p1 = (1.0, 2.0, 1.0)
p2 = (1.0, 1.0, 1.0)


roots2 (a,b,c) = if d < 0 then error "Sorry" else (x1, x2) -- handling exception!
        where 
                x1 = e + sqrt d / (2*a)
                x2 = e - sqrt d / (2*a)
                d = b*b-4*a*c
                e = -b/ (2*a)


-- ********************************************************************** list, how something so simple can be useful?


empty = []
lInt = [1,2]
lBool = [False, True]
lTupleInt = [(1,2), (2,3)]
lListInt = [[1,2],[1,2,3]]
lToHundred = [1 .. 100]
string = "name" -- string are list too, list of chars ['n','a','m','e'] -- slowpoke



nthListElem l 1 = head l
nthListElem l n = nthListElem (tail l) (n-1)


-- recursion: the fundamental principle of functional programming, 
        -- or "Who stole loops and counters"

-- factorial
fact 0 = 1                -- zero step
fact n = n * fact (n-1)   -- induction step

-- sumatorio
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs



-- basic functions for lists
-- head tail length reverse ++(concat) map filter foldr zip zipWith

alphabet = [ 'a' .. 'z' ]

-- code your name into ASCII numbers and back ( do you know what map does?)

map1 :: (a->b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs


code :: String -> [Int]
code x = map1 ord x

-- toAscii x = map ord x

-- calculation of the area of a polygon














































































