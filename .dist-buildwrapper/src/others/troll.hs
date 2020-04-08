




descartar :: Int -> [a] -> [a]
descartar x l = descartarAux  x l []

descartarAux :: Int -> [a] -> [a] -> [a]
descartarAux _ [] ret = ret
descartarAux x (a:xs) ret = descartarAux (x-1) xs (if(x < 0) then (ret ++ [a]) else ret)