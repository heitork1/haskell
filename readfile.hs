import Data.List (sort, group)

type Linha = String
type Doc = String
type Palavra = String
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas xs = zip [1..] xs
numeraPalavras :: [(Int, Linha)] -> [(Int,Palavra)]
numeraPalavras linhas = concatMap (\(numLinha, linha) -> zip (repeat numLinha) (words linha)) linhas
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar = sort
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar = map (\xs -> (map fst xs, snd (head xs))) . group

main :: IO ()
main = do
    conteudo <- readFile "alice_haskell.txt"
    print (agrupar (ordenar (numeraPalavras (numLinhas (lines conteudo))))) --print()é geral, putStrLn() é para strings
  
