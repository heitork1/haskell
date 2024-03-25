primo m 
  | m == 1 || m == 2 || m == 3 || m == 5 || m == 7 = True
  | m `mod` 2 == 0 || m `mod` 3 == 0 || m `mod` 5 == 0 = False
  | otherwise = True