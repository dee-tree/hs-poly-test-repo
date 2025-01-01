module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist l = case reverse l of
    [] -> REmpty
    (head : tail) -> listToRlist (reverse tail) :< head

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    showsPrec p rl = showParen (p > 10) $ showString "[" . showHelper rl . showString "]" where
        showHelper :: Show a => ReverseList a -> ShowS
        showHelper rl = case rl of
            REmpty -> showString ""
            (REmpty :< tail) -> showsPrec (p + 1) tail
            (head :< tail) -> showHelper head . showString "," . showsPrec (p + 1) tail
    -- show rl = shows rl ""

instance (Eq a) => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (ah :< at) (bh :< bt) = (at == bt) && ah == bh
    (==) _ _ = False
--     (/=) this other = not $ this == other

instance Semigroup (ReverseList a) where
    (<>) REmpty b = b
    (<>) a REmpty = a
    (<>) a (h :< t) = (a <> h) :< t

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (h :< t) = fmap f h :< f t

instance Applicative ReverseList where
    pure v = REmpty :< v
    (fh :< ft) <*> rl = (fh <*> rl) <> fmap ft rl
    _ <*> _ =  REmpty

instance Monad ReverseList where
    return v = REmpty :< v
    REmpty >>= f = REmpty
    (h :< t) >>= f = (h >>= f) <> f t
