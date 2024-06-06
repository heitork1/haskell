import Data.List (sortBy, groupBy, nub)
import Data.Function (on)
type Doc = String
type Linha = String
type Palavra = String

numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras linhasNumeradas = concatMap (\(numLinha, linha) -> [(numLinha, palavra) | palavra <- words linha]) linhasNumeradas
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas linhas = zip [1..] linhas
ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar = sortBy (\(_, p1) (_, p2) -> compare p1 p2)
agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar = map (\xs -> (map fst xs, snd (head xs))) . groupBy ((==) `on` snd)
eliminarRep :: [([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep = map (\(nums, palavra) -> (nub nums, palavra))

main :: IO ()
main = do
     conteudo <- readFile "alice_haskell.txt"
     print(eliminarRep(agrupar(ordenar(numeraPalavras(numLinhas(lines conteudo))))))
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
