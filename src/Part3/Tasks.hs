module Part3.Tasks where

import Util (notImplementedYet)
import Data.List ( group, maximumBy, sort )
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
-- ff = iterate -- easy way
ff f x = x : ff f (f x) -- less easy way

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = error "no digits"
mostFreq a = snd $ maximumBy (comparing fst) $ map (\an -> (length an, head an)) $ group $ sort $ split a
 where
    split :: [Int] -> [Int]
    split a = case a of
        [] -> []
        (head : tail) -> if head > 9 then lsdHead : split (otherHead : tail) else head : split tail
            where lsdHead = head `mod` 10
                  otherHead = head `div` 10

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = uniq' []
    where uniq' seen tosee = case tosee of
                       [] -> seen
                       (h:tail) -> if h `elem` seen then uniq' seen tail else uniq' (h:seen) tail

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = merge [] $ map (\e -> (f e, e)) l
    where insert (r, e) into = case into of
            [] -> [(r, [e])]
            (rr, values) : tail -> if r == rr then (rr, e : values) : tail else (rr, values) : insert (r, e) tail

          merge :: (Eq k) => [(k, [a])] -> [(k, a)] -> [(k, [a])]
          merge seen tosee = case tosee of
            [] -> seen
            ((r, e) : tail) -> merge (insert (r, e) seen) tail
