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
  
{-
import Data.Char (isAlpha, toLower)
import Data.List (sort, group, nub)

type Linha = String
type Doc = String
type Palavra = String

-- Atribui números de linha às linhas do documento
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas xs = zip [1..] xs

-- Remove caracteres de pontuação de uma linha e converte para minúsculas
limparLinha :: Linha -> Linha
limparLinha = map (\c -> if isAlpha c || c == ' ' then toLower c else ' ')

-- Numera as palavras com base no número da linha, removendo palavras com menos de 3 letras
numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras linhas = 
    concatMap (\(numLinha, linha) -> zip (repeat numLinha) (filter (\w -> length w >= 3) (words (limparLinha linha)))) linhas

-- Ordena as palavras numeradas
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar = sort

-- Agrupa palavras, listando os números das linhas onde ocorrem
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar = map (\xs -> (map fst xs, snd (head xs))) . group

-- Elimina repetições de números de linha
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep = map (\(linhas, palavra) -> (nub linhas, palavra))

-- Constrói o índice das palavras no documento
construirIndice :: Doc -> [([Int], Palavra)]
construirIndice = eliminarRep . agrupar . ordenar . numeraPalavras . numLinhas . lines

-- Função principal que lê o arquivo e imprime o índice
main :: IO ()
main = do
    conteudo <- readFile "alice_haskell.txt"
    let indice = construirIndice conteudo
    mapM_ print indice


-}
