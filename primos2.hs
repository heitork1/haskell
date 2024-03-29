primo :: Int -> Bool
primo n = primo' n (n-1)
  where
    primo' :: Int -> Int -> Bool
    primo' _ 1 = True
    primo' n m
      | n `rem` m == 0 = False
      | otherwise = primo' n (m-1)
