
somaPares 0 = 0 -- Caso base: quando n é 0, a soma é 0
somaPares n
  | n `mod` 2 == 0 = n + somaPares (n - 2) -- Se n for par (resto da divisão por 2 é 0), soma n e chama a função recursivamente com n-2
  | otherwise = somaPares (n - 1) -- Se n for ímpar, apenas chama a função recursivamente com n-1

-- Exemplos de uso:
-- somaPares 5 => 4 + 2 + 0 = 6
-- somaPares 8 => 8 + 6 + 4 + 2 + 0 = 20
