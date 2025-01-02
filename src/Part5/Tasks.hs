module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl facc acc [] = acc
myFoldl facc acc (head : tail) = myFoldl facc (facc acc head) tail

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr facc acc [] = acc
myFoldr facc acc (head : tail) = facc head $ myFoldr facc acc tail

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (mapper f) [] where
    mapper :: (a -> b) -> a -> [b] -> [b]
    mapper f elem tail = f elem : tail

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (concmapper f) [] where
    concmapper :: (a -> [b]) -> a -> [b] -> [b]
    concmapper f elem tail = f elem ++ tail

myConcat :: [[a]] -> [a]
myConcat = myFoldr concatter [] where
    concatter :: [a] -> [a] -> [a]
    concatter subl tail = subl ++ tail

myReverse :: [a] -> [a]
myReverse = myFoldr reverser [] where
    reverser :: a -> [a] -> [a]
    reverser elem tail = tail ++ [elem]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (filtrator p) [] where
    filtrator :: (a -> Bool) -> a -> [a] -> [a]
    filtrator p elem tail = if p elem then elem : tail else tail

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (divider p) ([], []) where
    divider :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
    divider p elem (ok, nok) = if p elem then (elem : ok, nok) else (ok, elem : nok)
