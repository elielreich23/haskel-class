import Data.List

type Word' = String
type Line = String
type Doc = String

data Tree = Node Word' [Int] Tree Tree|Leaf deriving Show

numLines xs = zip [1..] xs


allNumWords [] = []
allNumWords ((n,l):xs) =  zip (repeat n) (words l) ++ allNumWords xs

insOrd a [] = [a]
insOrd a (x:xs)|a < x = a:x:xs
               |a > x = x:insOrd a xs
               |otherwise = x:xs

ins p i Leaf = Node p [i] Leaf Leaf
ins p i (Node w ns esq dir)|p == w = Node w  (insOrd i ns) esq dir
                           |p < w = Node w ns (ins p i esq) dir
                           |otherwise = Node w ns esq (ins p i dir)  

mIndexTree [] = Leaf
mIndexTree ((n,w):xs) = ins w n (mIndexTree xs)
-- [(1, "ave"), (2, "caramelo")]
-- ins "ave" 1 (mIndexTree [(2, "caramelo")])
--              ins "caramelo" 2 (mIndexTree [])

imprimir Leaf = ""
imprimir (Node w ns esq dir) = imprimir esq ++ show w ++ "-" ++ show ns ++ "\n" ++ imprimir dir

makeIndexTree = do putStr "Digite o nome do arquivo: "
                   arq <- getLine
                   txt <- readFile arq
                   let tree = mIndexTree(allNumWords(numLines(lines txt)))
                   imp tree

imp Leaf = return ()
imp (Node w ns esq dir) = do imp esq
                             putStr (show w ++ "-" ++ show ns ++ "\n")
                             imp dir
