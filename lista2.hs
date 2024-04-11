pertence n [] = False
pertence n (x:xs) | n == x = True
                  | otherwise = pertence n (xs) 

intersecao [] _ = [] 
intersecao _ [] = []
intersecao (x:xs) (y:ys) | pertence x ys = x : intersecao xs ys
                         | otherwise = intersecao xs ys
