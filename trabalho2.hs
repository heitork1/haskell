-- Importa funções da biblioteca Data.Char
import Data.Char (isAlpha, toLower)

-- Define aliases para tipos de dados, facilitando a compreensão do código
type Doc = String
type Line = String
type Word' = String

-- Definição da árvore binária de pesquisa, que pode ser um nó (Node) contendo uma palavra, uma lista de números de linha, 
-- e duas sub-árvores (esquerda e direita) ou uma folha vazia (Leaf)
data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show

-- Função que separa um documento em linhas
separarLinhas :: Doc -> [Line]
separarLinhas doc = lines doc

-- Função que numera as linhas do documento, retornando uma lista de tuplas (número da linha, linha)
numLines :: [Line] -> [(Int, Line)]
numLines linhas = zip [1..] linhas

-- Função para limpar uma palavra, removendo caracteres não alfabéticos
filtrarPalavra :: Word' -> Word'
filtrarPalavra = filter isAlpha

-- Função para converter uma palavra para minúsculas
minusculas :: Word' -> Word'
minusculas = map toLower

-- Função que associa a cada ocorrência de uma palavra o número da linha
-- Entrada:  [(1, "hello world"), (2, "foo bar")] 
-- Saída:  [(1, "hello"), (1, "world"), (2, "foo"), (2, "bar")]
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords linhasNumeradas = concatMap (\(numLinha, linha) -> [(numLinha, minusculas (filtrarPalavra palavra)) | palavra <- words linha]) linhasNumeradas

-- Função que percorre a lista de tuplas (linha, palavra) e insere cada uma delas na árvore
mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((linha, palavra):resto) = ins linha palavra (mIndexTree resto)

-- Função que insere uma palavra e o número da linha em que ela ocorre na árvore
ins :: Int -> Word' -> Tree -> Tree
ins linha palavra Leaf = Node palavra [linha] Leaf Leaf
ins linha palavra (Node w linhas esq dir)
    | palavra < w  = Node w linhas (ins linha palavra esq) dir
    | palavra > w  = Node w linhas esq (ins linha palavra dir)
    | otherwise    = Node w (insOrd linha linhas) esq dir

-- Função que insere um elemento em uma lista ordenada, sem duplicatas
insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

-- Função de impressão em ordem da árvore, visitando a sub-árvore esquerda, imprimindo o nó atual e visitando a sub-árvore direita
printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node palavra linhas esq dir) = do
    printTree esq
    putStrLn $ palavra ++ ": " ++ show linhas
    printTree dir

-- Função principal que combina todas as funções anteriores para construir a árvore de índices
makeIndexTree :: Doc -> Tree
makeIndexTree = mIndexTree . allNumWords . numLines . separarLinhas

-- Função principal para testar, que lê um documento, cria a árvore de índices e imprime a árvore
main :: IO ()
main = do
    documento <- readFile "alice_haskell.txt"
    let arvore = makeIndexTree documento
    printTree arvore
