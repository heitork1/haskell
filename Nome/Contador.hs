module Contador
(
    contaLetras
    , contaVogais
) where
    
import Data.Char (isAlpha)
ehVogal :: Char -> Bool
ehVogal c = elem c "aeiouAEIOU"

contaVogais :: String -> Int
contaVogais palavra = length (filter ehVogal palavra)

contaLetras :: String -> Int
contaLetras palavra = length (filter isAlpha palavra)