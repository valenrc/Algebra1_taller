factorial :: Integer -> Integer
factorial n
    | n == 0 = 1
    | n  > 0 = n * factorial (n-1)
    | n  < 0 = undefined

fibonacci :: Int -> Int
fibonacci n 
    | n==0      = 0
    | n==1      = 1
    | n > 0     = fibonacci(n-1) + fibonacci(n-2)
    | otherwise = undefined

parteEntera :: Float -> Integer
parteEntera x
    | x < 1    = 0
    | x > 1    = 1 + parteEntera(x-1)
    | otherwise = undefined

multiplode3 :: Int -> Bool
multiplode3 n
    | n == 0 = True
    | n >  0 = multiplode3(n-3)
    | otherwise = False

sumaImpares :: Int -> Int
sumaImpares n -- (2*1-1) + (2*2-1) + ... + (2*i-1)  suma de todos los primeros i-nros impares desde 1
    | n == 0 = 0
    | n >  0 = sumaImpares(n-1) + (2*n-1)

medioFact :: Integer -> Integer
medioFact n
    | n == 0 || n == 1 = 1
    | n  > 0 = n * medioFact (n-2)

sumaDigitos :: Integer -> Integer
sumaDigitos n
    | n < 10 = n
    | n >= 10 = mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Integer -> Bool
digitosIguales n
    | n > 10 = n == 10