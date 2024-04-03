ehTriangulo a b c | a < b + c && b < a + c && c < a + b = True
                  | otherwise = False

tipoTriangulo a b c | a /= b && a /= c && b /= c = "Escaleno"
                    | a == b && b /= c && a /= c || a == c && b /= c && b /= a || b == c && a /= b && c /= a = "Isosceles"
                    | a == b && a == c && b == c = "Equilatero" 

triangulo a b c
    | ehTriangulo a b c = tipoTriangulo a b c
    | otherwise = "nao eh um triangulo"

somaPares 0 = 0 
somaPares n
  | n `mod` 2 == 0 = n + somaPares (n - 2) 
  | otherwise = somaPares (n - 1)

somaPot m 0 = m 
somaPot m n = (2^n)*m + somaPot m (n-1)

primo m 
  | m == 1 || m == 2 || m == 3 || m == 5 = True
  | m `mod` 2 == 0 || m `mod` 3 == 0 || m `mod` 5 == 0 = False
  | otherwise = True

regressaoPI 0 0 0 = 0 
regressaoPI n d p = 
  if d <= n
    then 
      p*(4/d) + regressaoPI n (d + 2) ((-1)*p)
    else 
      regressaoPI 0 0 0

seriePI n = regressaoPI n 1 1