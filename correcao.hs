--Nota 9.0

--1)
--foo 24 9 = foo (24-9) 9
--foo 15 9 = foo (15-9) 9 
--foo 6 9 = foo 6 (9-6)
--foo 6 3 = foo (6-3) 3
--foo 3 3 = 3

--2)
impares [] = []
impares (x:xs) = if x `mod` 2 == 0 then impares xs else x : impares xs

--3)
remover _ [] = []
remover m (x:xs) = if m == x then remover m xs else x : remover m xs

--4)
todos [] = True
todos (x:xs) = if x == True then todos xs else False

--5)
segundos [] = []
segundos ((x,y):xs) = y : segundos xs