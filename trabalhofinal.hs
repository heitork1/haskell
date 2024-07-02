import Data.Char (isAlpha, toLower)

type Doc = String
type Line = String
type Word' = String

data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show

separarLinhas :: Doc -> [Line]
separarLinhas doc = lines doc

numLines :: [Line] -> [(Int, Line)]
numLines linhas = zip [1..] linhas

filtrarPalavra :: Word' -> Word'
filtrarPalavra = filter isAlpha
-- Função para converter uma palavra para minúsculas
minusculas :: Word' -> Word'
minusculas = map toLower

allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords linhasNumeradas = concatMap (\(numLinha, linha) -> [(numLinha, minusculas (filtrarPalavra palavra)) | palavra <- words linha]) linhasNumeradas

mIndexTree :: [(Int, Word')] -> Tree
mIndexTree [] = Leaf
mIndexTree ((linha, palavra):resto) = ins linha palavra (mIndexTree resto)

ins :: Int -> Word' -> Tree -> Tree
ins linha palavra Leaf = Node palavra [linha] Leaf Leaf
ins linha palavra (Node w linhas esq dir)
    | palavra < w  = Node w linhas (ins linha palavra esq) dir
    | palavra > w  = Node w linhas esq (ins linha palavra dir)
    | otherwise    = Node w (insOrd linha linhas) esq dir

insOrd :: Ord a => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

printTree :: Tree -> IO ()
printTree Leaf = return ()
printTree (Node palavra linhas esq dir) = do
    printTree esq
    putStrLn $ palavra ++ ": " ++ show linhas
    printTree dir

makeIndexTree :: Doc -> Tree
makeIndexTree = mIndexTree . allNumWords . numLines . separarLinhas

main :: IO ()
main = do
    documento <- readFile "alice_haskell.txt"
    let arvore = makeIndexTree documento
    printTree arvore
