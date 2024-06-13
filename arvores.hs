data Arvore = Folha | No Int Arvore Arvore deriving Show

arvB = No 4 (No 2 Folha Folha)
            (No 10 (No 5 Folha Folha)
                   (No 15 Folha Folha))

--Acessar todos os nós e colocar numa lista
todosnos Folha = []
todosnos (No n Folha Folha) = [n]
todosnos (No n esq dir) = n : todosnos esq ++ todosnos dir

--Acessar somente as folhas e colocar numa lista
folhas Folha = []
folhas (No n Folha Folha) = [n]
folhas (No n esq dir) = folhas esq ++ folhas dir

--Acessar somente as folhas e colocar numa lista
soNos Folha = []
soNos (No n Folha Folha) = []
soNos (No n esq dir) = n : soNos esq ++ soNos dir

data Arvp a = Folhap | Nop a (Arvp a) (Arvp a) deriving Show
-- não deu certo embaixo:
arvP = Nop (4, "nó 1") (Nop (2, "No 2") (Nop (1, "No 3") Folhap Folhap) Folhap)
            (Nop (10, "no 4") (Nop (5, "no 5") Folhap Folhap))
                   (Nop (15, "no 6") Folhap Folhap)
----------------------------------------------------------------------------------------------------
data Arvore = Folha | No Int Arvore Arvore deriving Show

arvB = No 4 (No 2 (No 1 Folha Folha) Folha)
            (No 10 (No 5 Folha Folha)
                   (No 15 Folha Folha))

todosnos Folha = []
todosnos (No n Folha Folha) = [n]
todosnos (No n esq dir) = n : todosnos esq ++ todosnos dir

folhas Folha = []
folhas (No n Folha Folha) = [n]
folhas (No n esq dir) = folhas esq ++ folhas dir

soNos Folha = []
soNos (No n Folha Folha) = []
soNos (No n esq dir) = n : soNos esq ++ soNos dir

data Arvp a = Folhap | Nop a (Arvp a) (Arvp a) deriving Show

arvP = Nop (4, "no 1") (Nop (2, "No 2") (Nop (1, "No 3") Folhap Folhap) Folhap)
            (Nop (10, "no 4") (Nop (5, "no 5") Folhap Folhap)
                   (Nop (15, "no 6") Folhap Folhap))

arvP2 = Nop 4.5 (Nop 2.1 (Nop 1.0 Folhap Folhap) Folhap)
                (Nop 10.3 (Nop 5.4 Folhap Folhap)
                    (Nop 15.1 Folhap Folhap))

insere x = Nop x Folhap Folhap
insereArv x Folhap = insere x
insereArv x (Nop n esq dir)
    | x == n = Nop x esq dir 
    | x < n  = Nop n (insereArv x esq) dir
    | x > n  = Nop n esq (insereArv x dir)

arvP3 = Nop 4 (Nop 2 Folhap Folhap)
            (Nop 10 (Nop 5 Folhap Folhap)
                   (Nop 15 Folhap Folhap))

arvP4 = foldr insereArv Folhap [1,10,5,2,4]
