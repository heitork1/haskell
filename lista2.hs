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

nUltimos [] = []
nUltimos n (x:xs) | n >= length x:xs = x:xs
               | otherwise = nUltimos n (x:xs - last xs)

--nUltimos :: Int -> [a] -> [a]
--nUltimos _ [] = []  -- Se a lista de entrada for vazia, retorna uma lista vazia
--nUltimos n xs
  --  | n >= length xs = xs  -- Se n for maior ou igual ao comprimento de xs, retorna xs inteira
    -- | otherwise = drop (length xs - n) xs  -- Remove os primeiros length xs - n elementos de xs
