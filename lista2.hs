-- só posso utilizar : para adicionar os valores a esquerda

pertence n [] = False
pertence n (x:xs) | n == x = True
                  | otherwise = pertence n xs

intersecao [] _ = []
intersecao _ [] = []
intersecao (x:xs) (y:ys) | pertence x ys = x : intersecao xs ys
                         | otherwise = intersecao xs ys
inverso [x] = [x]
inverso (x:xs) = inverso xs ++ [x]

nUltimos _ [] = []
nUltimos n (x:xs) | n >= length (x:xs) = (x:xs)
               | otherwise = nUltimos n xs 

soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y) : soma2 xs ys 


pot2 n = [2^x | x <- [1..n]]

intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys) | x <= y = x : intercalacao xs (y:ys)
                          | otherwise = y : intercalacao (x:xs)ys

menor [x] = x
menor (x:y:xs) = if x <= y then menor (x:xs) else menor (y:xs)

removerElem _ [] = [] 
removerElem y (x:xs)
    | y == x = xs 
    | otherwise = x : removerElem y xs 

ordenar [] = []
ordenar (x:xs) = menor (x:xs) : ordenar (removerElem (menor (x:xs))(x:xs))  

insereOrd n [] = [n]
insereOrd n (x:xs) | pertence n (x:xs) = (x:xs)
                   | n <= x = n : (x:xs)
                   | otherwise = x : insereOrd n xs

enesimo 1 (x:_) = x  -- Caso base: se n for 1, retorna o primeiro elemento da lista
enesimo n (_:xs) = enesimo (n - 1) xs  -- Recursivamente, diminui n e avança para o próximo elemento

--13
repetir 0 _ = []
repetir n e = e : repetir (n - 1) e

--14
removeTab [] = []
removeTab (x:xs)
    | x == '\t' = ' ' : removeTab xs 
    | otherwise = x : removeTab xs

--15
minusculas [] = []
minusculas (x:xs) | x == 'A' = 'a' : minusculas xs
                | x == 'B' = 'b' : minusculas xs  
                | x == 'C' = 'c' : minusculas xs
                | x == 'D' = 'd' : minusculas xs
                | x == 'E' = 'e' : minusculas xs
                | x == 'F' = 'f' : minusculas xs
                | x == 'G' = 'g' : minusculas xs
                | x == 'H' = 'h' : minusculas xs
                | x == 'I' = 'i' : minusculas xs
                | x == 'J' = 'j' : minusculas xs
                | x == 'K' = 'k' : minusculas xs
                | x == 'L' = 'l' : minusculas xs
                | x == 'M' = 'm' : minusculas xs
                | x == 'N' = 'n' : minusculas xs
                | x == 'O' = 'o' : minusculas xs
                | x == 'P' = 'p' : minusculas xs
                | x == 'Q' = 'q' : minusculas xs
                | x == 'R' = 'r' : minusculas xs
                | x == 'S' = 's' : minusculas xs
                | x == 'T' = 't' : minusculas xs
                | x == 'U' = 'u' : minusculas xs
                | x == 'V' = 'v' : minusculas xs
                | x == 'W' = 'w' : minusculas xs
                | x == 'X' = 'x' : minusculas xs
                | x == 'Y' = 'y' : minusculas xs
                | x == 'Z' = 'z' : minusculas xs
                | otherwise = x : minusculas xs

--16
inversoDupla [] = []
inversoDupla ((x, y):xs) = (y, x) : inversoDupla xs 

--17
simetrico [] = []
simetrico ((x,y):xs) = if x == y then True : simetrico xs else False : simetrico xs
