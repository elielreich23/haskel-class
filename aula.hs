--Aula 3: Recursão
fat 0 = 1
fat n = n * fat(n-1)

fat' n = if n == 0 then 1 else n * fat(n-1)

pot n 0 = 1
pot n e = n * pot n (e-1)

pot' n e = if e == 0 then 1 else n * pot' n (e - 1)

mdc a b = if a == b then a else if a > b then mdc (a - b) b else mdc a (b-a)

--Aula 5: Listas
tam [] = 0
tam (x:xs) = 1 + tam xs

somatorio [] = 0 -- 0 é o elemento neutro da operação de soma
somatorio (x:xs) = x + somatorio xs

produtorio [] = 1 -- 1 é o elemento neutro da operação de multiplicação
produtorio (x:xs) = x * produtorio xs

eh [] = True -- True é o elemento neutro da operação && 
eh (x:xs) = x && and xs

soma _ [] = []
soma [] _ = []
soma (x:xs) (y:ys) = x + y : soma xs ys

soma' [] [] = []
soma' a [] = a
soma' [] a = a
soma' (x:xs) (y:ys) = x + y : soma xs ys

maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs --A função "maior xs" é executada duas vezes (ruim)

maior' [x] = x
maior' (x:xs) = if x > m then x else m
    where m = maior xs -- A função "maior xs" é executada só uma vez (bom)

--Aula 6:
pertence e [] = False
pertence e (x:xs) = if x == e then True else pertence e xs

inter [] ys = []
inter (x:xs) ys = if pertence x ys then x:inter xs ys else inter xs ys --ignorou o x pq não pertence

concatenar [] (y:ys) = ys
concatenar (x:xs) (y:ys) = x:concatenar xs (y:ys)

ins a [] = [a]
ins a (x:xs) = if a == x then (x:xs) 
                 else if a < x then a:x:xs 
                        else x:ins a xs

ins' a [] = [a]
ins' a l@(x:xs) = if a == x then l 
                 else if a < x then a:l
                        else x:ins a xs --A lista inicial pode ser chamada de l ou x:xs

ins'' e [] = []
ins'' e l@(x:xs)| e == x = l
                | e < x = e:l
                | otherwise = x:ins e xs --Quando se tem mais de dois testes

--Aula 7:
nprimeiros _ [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

inv [] = []
inv (x:xs) = inv xs ++ [x]

--Aula 8: Compreensão de listas
--listas infinitas e lazy evaluation
npares n = nprimeiros n [2*x|x <- [0..]]

remover' _ [] = []
remover' a (x:xs) = if a == x then remover' a xs else x:remover' a xs --Lame way

--remover a xs = [x|x <- xs, x /= a] --Cool way

pot2 n = [2^x|x<-[0..n]]

--Aula 9: Tuplas
primeiro (x,y) = x

segundo (x,y) = y

adicao x y = x + y

adicao' (x,y) = x + y

juntar _ [] = []
juntar [] _ = []
juntar (x:xs) (y:ys) = (x,y) : juntar xs ys

procurar e [] = ""
procurar e ((n,t):ls) = if e == n then t else procurar e ls

tab' _ [] = []
tab' (x:xs) (y:ys) = (x, y, x*y): tab' (x:xs) ys

tab [] _ = []
tab (x:xs) (y:ys) = tab' (x:xs) (y:ys): tab xs (y:ys)

tab'' xs ys = [(x,y,x*y)|x<-xs, y<-ys]

--Aula 10:
separar [] = ([],[])
separar ((x,y):xys) =(x:xs, y:ys) 
    where (xs,ys) = separar xys
