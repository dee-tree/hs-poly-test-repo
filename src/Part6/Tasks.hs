{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Prelude hiding (lookup)
import Data.Map
import qualified Data.Maybe

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       width :: mx -> Int
       height :: mx -> Int
       element :: Int -> Int -> mx -> Int
       e :: Int -> mx
       z :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       width _ = 1
       height _ = 1
       element 0 0 a = a
       element _ _ _ = error "Out of bounds"
       e 1 = 1
       e n = error "Out of bounds"
       z 1 1 = 0
       z _ _ = error "Out of bounds"

instance Matrix [[Int]] where
       width [] = 0
       width a = length $ head a
       height a = length a
       element i j a = case () of
              _ | i < 0 || j < 0 -> 0
                | i >= height a -> 0
                | j >= width a -> 0
              _ -> a !! i !! j

       e n = [
        [fromEnum $ x == y | y <- [1..n]]
              | x <- [1..n]]

       z n m = [
        [0 | y <- [1..n]]
              | x <- [1..m]]

instance Matrix (SparseMatrix Int) where
       width a = sparseMatrixWidth a
       height a = sparseMatrixHeight a
       element i j a = valorzero where
              mbvalue = lookup (i, j) $ sparseMatrixElements a
              valorzero = Data.Maybe.fromMaybe 0 mbvalue

       e n = SparseMatrix n n (fill n empty) where
              fill :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
              fill 0 mp = mp
              fill n mp = fill (n - 1) $ insert (n-1, n-1) 1 mp

       z n m = SparseMatrix n n empty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = e

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = z

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
