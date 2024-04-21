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
