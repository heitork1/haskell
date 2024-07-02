-- Define um sinônimo de tipo para String
type Doc = String

-- Define um sinônimo de tipo para String, representando uma linha de texto
type Line = String

-- Define um sinônimo de tipo para String, representando uma palavra
type Word' = String

-- Definição do tipo de dados para uma árvore binária de pesquisa
data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show
-- Um nó pode conter uma palavra, uma lista de números de linha onde essa palavra ocorre,
-- e duas subárvores (esquerda e direita), ou pode ser uma folha (Leaf).

-- Função que separa um documento em linhas de texto
separarLinhas :: Doc -> [Line]
separarLinhas doc = lines doc

-- Função que enumera as linhas do documento
numLines :: [Line] -> [(Int, Line)]
numLines linhas = zip [1..] linhas
-- Cria uma lista de tuplas, onde cada tupla contém um número de linha e uma linha correspondente.

-- Função que associa cada palavra a sua linha de ocorrência no documento
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords linhasNumeradas = concatMap (\(numLinha, linha) -> [(numLinha, palavra) | palavra <- words linha]) linhasNumeradas
-- Usa concatMap para transformar cada tupla (número de linha, linha) em uma lista de tuplas (número de linha, palavra),
-- onde palavra é uma palavra extraída da linha usando 'words'.

-- Função que insere um elemento em uma lista ordenada, sem duplicatas
insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys
-- Insere x em ys mantendo a ordem, se x já estiver presente, a lista não será modificada.

-- Função que insere uma palavra e sua linha correspondente na árvore
ins :: Int -> Word' -> Tree -> Tree
ins linha palavra Leaf = Node palavra [linha] Leaf Leaf
-- Caso base: insere um novo nó se a árvore estiver vazia.

ins linha palavra (Node w linhas esq dir)
    | palavra < w  = Node w linhas (ins linha palavra esq) dir
    | palavra > w  = Node w linhas esq (ins linha palavra dir)
    | otherwise    = Node w (insOrd linha linhas) esq dir
-- Insere recursivamente em uma árvore não vazia, mantendo a ordem alfabética das palavras.

-- Função que constrói a árvore de índice a partir de uma lista de tuplas (número de linha, palavra)
mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((linha, palavra):resto) = ins linha palavra (mIndexTree resto)
-- Usa foldr para inserir cada tupla (linha, palavra) na árvore, começando com uma árvore vazia (Leaf).

-- Função para imprimir uma árvore em ordem
printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node palavra linhas esq dir) = do
    printTree esq
    putStrLn $ palavra ++ ": " ++ show linhas
    printTree dir
-- Imprime recursivamente os nós da árvore em ordem, mostrando a palavra e suas linhas de ocorrência.

-- Função principal que combina todas as etapas para criar a árvore de índice
makeIndexTree :: Doc -> Tree
makeIndexTree = mIndexTree . allNumWords . numLines . separarLinhas
-- Composição de funções: separa o documento em linhas, enumera as linhas, associa palavras com linhas e
-- insere na árvore de índice.

-- Função principal que lê um arquivo, constrói a árvore de índice e a imprime
main :: IO ()
main = do
    documento <- readFile "alice_haskell.txt"
    let arvore = makeIndexTree documento
    printTree arvore
-- Lê o conteúdo do arquivo "alice_haskell.txt", cria a árvore de índice com makeIndexTree e a imprime usando printTree.
