raiz1 a b c =
    if b**2-4*a*c<0
        then error "Nao ha raizes reais"
        else (-b+sqrt(b**2-4*a*c))/(2*a)

raiz2 a b c =
    if b**2-4*a*c<0
        then error "Nao ha raizes reais"
        else (-b-sqrt(b**2-4*a*c))/(2*a)

raizes a b c | b**2-4*a*c<0 = error "Nao ha raizes reais"
             | otherwise = "As raizes sao: " ++ show(raiz1 a b c) ++ " " ++ show(raiz2 a b c)


raiz3 a b c | b**2-4*a*c<0 = error "Nao ha raizes reais"
            | otherwise = raiz1 a b c 
 