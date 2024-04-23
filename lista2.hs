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
