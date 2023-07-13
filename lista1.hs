    --exo 1
ehTriangulo a b c = if a < b + c && b < a + c && c < a + b then True else False

--exo 2
tipoTriangulo x y z = if x == y && y == z then "equilatero"
else if x == y || y == z || x == z then "isoceles" else "escaleno" 

--exo 3
triangulo a b c = if ehTriangulo a b c then if a == b && b == c then "equilatero"
else if a == b || b == c|| a == c then "isoceles" else "escaleno" 
else "nao eh um triangulo"

--exo4
par a = if rem a 2 == 0 then a else a - 1
somaPares 0 = 0
somaPares a = par a + somaPares (par a - 2)

--exo 5
somaPot2m m 0 = m
somaPot2m m n = 2^n * m  +somaPot2m m (n - 1)

--exo 6
ehdiv n 1 = True
ehdiv n m = if rem n m == 0 then False else ehdiv n (m-1)
primo n = ehdiv n(n-1)

--ADICIONAL CODE 
    --exo 1
isTriangulo a b c = if a + b>c && b+c>a && c+a>b then True else False

--exo 2
typeTriangle x y z = if x == y && y == z then "equilatero"
else if x == y || y == z || x == z then "isoceles" else "escaleno" 

--exo3
triangle a b c = if isTriangulo a b c then if a == b && b == c  then "equilatero"
else if a == b || b == c || c == a then "isoceles" else "escaleno"
else "not a triangle"

--exo4
prime a = if rem a 2 == 0 then a else a-1
sumEven 0 = 0
sumEven a = prime a + sumEven (prime a - 2)

--exo5
somaPot2m m 0 = m
somaPot2m m n = 2^n * m +somaPot2m m (n-1)

--exo6
ehdiv n 1 = True
ehdiv n m = if rem n m == 0 then False else ehdiv n (m-1)
primo n = ehdiv n(n-1)

pares [] = 0
pares (x:xs) = if rem x 2 == 0 then x else if x:pares xs

occorence _ [] = 0
occorence n(x:xs) = if n == x then 1 +occorence n xs else occorence n xs
