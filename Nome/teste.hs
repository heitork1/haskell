import Contador (contaLetras, contaVogais)

main :: IO ()
main = do
    let palavra = "Hello, world!"
    putStrLn $ "Quantidade de letras: " ++ show (contaLetras palavra)
    putStrLn $ "Quantidade de vogais: " ++ show (contaVogais palavra)
