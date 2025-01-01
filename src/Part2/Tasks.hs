module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 5 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 5 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 6 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
   IntConstant _ -> expression
   Variable name -> if name == varName then replacement else expression
   BinaryTerm op l r -> BinaryTerm op (replaceVar varName replacement l) (replaceVar varName replacement r)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of 
   IntConstant _ -> expr
   Variable _ -> expr
   BinaryTerm Plus (IntConstant l) (IntConstant r) -> IntConstant $ l + r
   BinaryTerm Minus (IntConstant l) (IntConstant r) -> IntConstant $ l - r
   BinaryTerm Times (IntConstant l) (IntConstant r) -> IntConstant $ l * r
   BinaryTerm op l r -> case (evl, evr) of
      (IntConstant _, IntConstant _) -> evaluate $ BinaryTerm op evl evr
      _ -> BinaryTerm op evl evr
      where evl = evaluate l
            evr = evaluate r
