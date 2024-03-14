somaPares :: Int -> Int
somaPares n = somaParesAux n 0

somaParesAux :: Int -> Int -> Int
somaParesAux n acc
    | n == 0    = acc
    | even n    = somaParesAux (n - 1) (acc + n)
    | otherwise = somaParesAux (n - 1) acc
