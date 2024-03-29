seriePI :: Int -> Double  -- `seriePI` takes an Int (number of terms) and returns a Double (approximation)
seriePI n = calcularSerie 0 1 n  -- Pass 'n' to the function

calcularSerie soma termo n  -- Include 'n' as an argument
  | 4 / termo < 4 / fromIntegral n =  -- Check condition using 'n'
    do  -- 'do' block for let binding
      novoTermo <- calcularSerie (soma + (if n `mod` 2 == 0 then - (4 / termo) else 4 / termo)) (termo + 2) n  -- Pass 'n' in recursive call
      novoTermo  -- Return the newly calculated term (Double)
  | otherwise = soma  -- If condition not met, return current soma

main :: IO ()
main = do
  let pi_aprox_100 = seriePI 100
  let pi_aprox_10000 = seriePI 10000
  putStrLn $ "Aproximação para pi com n = 100: " ++ show pi_aprox_100
  putStrLn $ "Aproximação para pi com n = 10000: " ++ show pi_aprox_10000
  putStrLn $ "Diferença com pi (n = 100): " ++ show (abs (pi - pi_aprox_100))
  putStrLn $ "Diferença com pi (n = 10000): " ++ show (abs (pi - pi_aprox_10000))
