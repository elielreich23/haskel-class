--Exercício 1
pertence n [] = False
pertence n (x:xs) = if n == x then True
                    else pertence n xs 

--Exercício 2
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

--Exercício 3
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--Exercício 4
nprimeiros _ [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

nUltimos n l@(x:xs) = nprimeiros n (inverso l)

--Exercício 5
soma2 _ [] = []
soma2 [] _ = []
soma2 (x:xs) (y:ys) = x + y:soma2 xs ys

soma2' [] [] = [] --Versão que mantém a lista maior
soma2' (x:xs) [] = x:xs
soma2' [] (y:ys) = y:ys
soma2' (x:xs) (y:ys) = x + y:soma2 xs ys

--Exercício 6
pot2'' 0 = [1]
pot2'' n = (2^n):(pot2'' (n-1))

pot2' n = inverso (pot2' n)

pot2 n = [2^x|x<-[0..n]]

--Exercício 7
intercalacao xs [] = xs
intercalacao [] ys = ys
intercalacao (x:xs) (y:ys) = if x <= y then x:intercalacao xs (y:ys) else y:intercalacao (x:xs) ys

--Exercício 8
menor [x] = x
menor (x:xs) = if x < m then x else m
    where m = menor xs

--Exercício 9
removerElem _ [] = []
removerElem a (x:xs) = if a == x then xs else x:removerElem a xs

--Exercício 10
ordenar [] = []
ordenar l@(x:xs) = [menor l] ++ ordenar (removerElem (menor l) l)

--Exercício 11
ins e [] = []
ins e l@(x:xs)| e == x = l
              | e < x = e:l
              | otherwise = x:ins e xs

--Exercício 12
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

--Exercício 13
repetir 0 e = []
repetir n e = (e:repetir (n-1) e)

--Exercício 14
numString'' 0 = '0'
numString'' 1 = '1'
numString'' 2 = '2'
numString'' 3 = '3'
numString'' 4 = '4'
numString'' 5 = '5'
numString'' 6 = '6'
numString'' 7 = '7'
numString'' 8 = '8'
numString'' 9 = '9'
--numString'' :: Int -> Char
--numString'' x = toEnum (x+48)

numString' 0 = ""
numString' x = numString'' (rem x 10) : numString' (div x 10)

numString x = inverso(numString' x)

--Exercício 15
stringNum' '0' = 0
stringNum' '1' = 1
stringNum' '2' = 2
stringNum' '3' = 3
stringNum' '4' = 4
stringNum' '5' = 5
stringNum' '6' = 6
stringNum' '7' = 7
stringNum' '8' = 8
stringNum' '9' = 9

stringNum [] = 0
stringNum (x:xs) = (stringNum' x)*10^(length (xs)) + stringNum xs

--Exercício 16
bin2int' [] _ = 0
bin2int' (x:xs) y = (stringNum' x)*2^y + bin2int' xs (y+1) 

bin2int (x:xs) = bin2int' (inverso (x:xs)) 0

--Exercício 17
int2bin' 0 = []
int2bin' x = numString'' (rem x 2): int2bin'(div x 2)

int2bin x = inverso(int2bin' x)

--Exercício 18
minusculas [] = []
minusculas (x:xs) = if fromEnum x > 64 && fromEnum x < 91 then toEnum ((fromEnum x)+32): minusculas xs else x:minusculas xs