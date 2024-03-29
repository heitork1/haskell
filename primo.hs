
primo n
    | n /= 2 && n /= 3 && n /= 5 && n `mod` 2 == 0 && n `mod` 3 == 0  && n `mod` 5 == 0 = False
    | otherwise = True