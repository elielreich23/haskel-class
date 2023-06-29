
import Data.List

--função lines

numLines xs = zip [1..] xs --posso omitir o xs 

allNumWords [] = []
allNumWords ((n,l):xs) =  zip (repeat n) (words l) ++ allNumWords xs

inv [] = []
inv ((x,y):xs) = (y,x):inv xs

sortLs xs = inv(sort(inv xs))

amalgamate [] = []
amalgamate ((n,w):xs) = ([a|(a,b) <- ((n,w):xs), b == w],w): amalgamate ([(a,b)|(a,b) <- xs, b /= w])

aux [] = []
aux (x:xs) = x:aux [a|a <- xs, a /= x]

shorten [] = []
shorten ((ns, w):xs) = (aux ns, w):shorten xs

imprimir [] = putStr "\n"
imprimir ((l,s):xs) = do putStr (show l ++ "-")
                         putStr (s ++ "\n")
                         imprimir xs

main = do putStr "Arquivo:"
          arq <- getLine
          txt <- readFile arq
          let linhanum = shorten(amalgamate(sortLs(allNumWords(numLines(lines txt)))))
          imprimir linhanum

