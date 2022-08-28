--1)
absoluto :: Int -> Int
absoluto x | x < 0 = x*(-1)
           | otherwise = x

--2)
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m | absoluto n >= absoluto m = n
                   | otherwise = m

--3)
maximo :: Int -> Int -> Int
maximo a b | a>=b = a
           | otherwise = b
maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c| c > maximo a b = c
             | otherwise = maximo a b

--4)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x==0 || y==0

algunoEs0_pm :: Float -> Float -> Bool
algunoEs0_pm 0 _ = True
algunoEs0_pm _ 0 = True
algunoEs0_pm x y = False

--5)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x==0 && y==0

ambosSon0_pm :: Float -> Float -> Bool
ambosSon0_pm 0 0 = True
ambosSon0_pm x y = False

--6)
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | mod a b == 0 = True
                 | otherwise = False

--7)
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

--8)
digitoDecenas:: Int -> Int
digitoDecenas x = mod (div x 10) 10 