raiz1 a b c =
    if b**2-a*c<0
        then error "Nao ha raizes reais"
        else (-b+sqrt(b**2-4*a*c))/2*a

raiz2 a b c =
    if b**2-a*c<0
        then error "Nao ha raizes reais"
        else (-b-sqrt(b**2-4*a*c))/2*a
