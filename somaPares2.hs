
somaPares 0 = 0 
somaPares n
  | n `mod` 2 == 0 = n + somaPares (n - 2) 
  | otherwise = somaPares (n - 1)
