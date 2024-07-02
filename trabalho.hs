type Doc = String
type Line = String
type Word' = String

-- Definição da árvore binária de pesquisa
data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show

-- A função 'separarLinhas' separa um documento em linhas
separarLinhas :: Doc -> [Line]
separarLinhas doc = lines doc

-- A função 'numLines' numera as linhas do documento
numLines :: [Line] -> [(Int, Line)]
numLines linhas = zip [1..] linhas

-- A função 'allNumWords' associa a cada ocorrência de uma palavra o número da linha
-- entrada:  [(1, "hello world"), (2, "foo bar")] 
-- saída:  [(1, "hello"), (1, "world"), (2, "foo"), (2, "bar")].
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords linhasNumeradas = concatMap (\(numLinha, linha) -> [(numLinha, palavra) | palavra <- words linha]) linhasNumeradas

-- A função 'insOrd' insere um elemento em uma lista ordenada, sem duplicatas
insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

-- A função 'ins' insere uma palavra e o número da linha em que ela ocorre na árvore
ins :: Int -> Word' -> Tree -> Tree
ins linha palavra Leaf = Node palavra [linha] Leaf Leaf
ins linha palavra (Node w linhas esq dir)
    | palavra < w  = Node w linhas (ins linha palavra esq) dir
    | palavra > w  = Node w linhas esq (ins linha palavra dir)
    | otherwise    = Node w (insOrd linha linhas) esq dir

-- A função 'mIndexTree' percorre a lista de tuplas (linha, palavra) e insere cada uma delas na árvore
mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((linha, palavra):resto) = ins linha palavra (mIndexTree resto)

-- Função de impressão em ordem da árvore
printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node palavra linhas esq dir) = do
    printTree esq
    putStrLn $ palavra ++ ": " ++ show linhas
    printTree dir

-- Função principal que combina todas as funções anteriores para construir a árvore de índices
makeIndexTree :: Doc -> Tree
makeIndexTree = mIndexTree . allNumWords . numLines . separarLinhas

-- Função principal para testar
main :: IO ()
main = do
    documento <- readFile "alice_haskell.txt"
    let arvore = makeIndexTree documento
    printTree arvore
