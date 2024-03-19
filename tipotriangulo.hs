tipoTriangulo a b c | a /= b && a /= c && b /= c = "Escaleno"
                    | a == b && b /= c && a /= c || a == c && b /= c && b /= a || b == c && a /= b && c /= a = "Isosceles"
                    | a == b && a == c && b == c = "Equilatero" 