
seriePI n = calcSerie 1 1
  where
    calcSerie termo denominador
      | termo <= n && 4 / denominador > 4 / fromIntegral n = 0
      | even termo = 4 / denominador - calcSerie (termo + 1) (denominador + 2)
      | otherwise = -(4 / denominador) + calcSerie (termo + 1) (denominador + 2)

-- seriePI :: Int -> Double
-- seriePI n
--   | n == 1 = 4.0
--   | otherwise = 4.0 / fromIntegral n + seriePI (n - 2)