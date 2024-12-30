module Part1.Tasks where

import Util(notImplementedYet)

circle = 2 * pi

-- normalize in circle bounds
normic x = if abs x < circle then x else normic (x - 1 * signum x * circle)


-- @ arg x - trigonomic function argument
-- @ arg acc - current fun value
-- @ arg prev - previous computed summand
-- @ arg i - current iteration
-- @ arg fdenom - denominator computational function
sinCosTaylor x acc prev i fdenom =
    let circle = 2 * pi
        eps = 0.0000001 -- precision i

        numerator = prev * x * x
        denumenator = fdenom i
        current = -numerator / denumenator in

        if abs (current - prev) <= eps then acc
        else sinCosTaylor x (acc + current) current (i + 1) fdenom

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinCosTaylor normx normx normx 1 denominator
    where
        normx = normic x
        denominator i = 2 * i * (2 * i + 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sinCosTaylor normx 1 1 1 denominator
    where
        normx = normic x
        denominator i = (2 * i - 1) * 2 * i

-- Orrrr...
-- myCos x = cos' 1 1 1
--     where
--         circle = 2 * pi 
--         normalize x = if abs x < circle then x else normalize (x - 1 * signum x * circle)
--         xnorm = normalize x
--         eps = 0.0000001 -- precision

--         -- @ arg acc - current cos value
--         -- @ arg prev - previous computed summand
--         -- @ arg i - current iteration
--         cos' acc prev i = 
--             let numerator = prev * xnorm * xnorm
--                 denumenator = 2 * i * (2 * i - 1)
--                 current = -numerator / denumenator in

--             if abs (current - prev) <= eps then acc
--             else cos' (acc + current) current (i + 1)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b | a < 0 || b < 0 = myGCD posa posb
          | a == b = a
          | b == 0 = a
          | b > a = myGCD b a
          | otherwise = myGCD b areduced
          where
            posa = abs a
            posb = abs b
            areduced = a `rem` b


isLeap y = y `mod` 400 == 0 || y `mod` 4 == 0 && y `mod` 100 /= 0

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y | m <= 0 || 12 < m = False
                    | d <= 0 = False
                    | m == 2 && d == 29 && isLeap y = True
                    | m == 2 && 29 <= d = False
                    | d == 31 = m < 8 && odd m || 7 < m && even m
                    | d < 31 = True
                    | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base 0 = 1
myPow base 1 = base
myPow base pow = pow' base pow 1
    where pow' _ 0 acc = acc
          pow' base pow acc = pow' base (pow - 1) $ acc * base


isqrt = floor . sqrt . fromIntegral

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = isPrime' n 2
    where upperBound = 1 + isqrt n
          isPrime' n d | d == upperBound = d * d /= n
                       | n `mod` d == 0 = False
                       | otherwise = isPrime' n $ d + 1

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = (/ 2) $ abs $ area points $ head points
    where summator :: Point2D -> Point2D -> Double
          summator a b = case (a, b) of
            ((ax, ay), (bx, by)) -> ax * by - bx * ay
          area points hd = case points of
            [] -> 0
            [h] -> 0
            [h, n] -> summator h n + summator n hd
            (h:n:tail) -> summator h n + area (n:tail) hd

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | maxi >= mid + mini = -1 -- does not exist
    | smaxi == smini + smid = 2
    | smaxi < smini + smid && smid < smaxi + smini && smini < smaxi + smid = 1
    | otherwise = 0
    where maxi = maximum [a, b, c]
          mini = minimum [a, b, c]
          mid = a + b + c - maxi - mini
    
          smaxi = maxi * maxi
          smini = mini * mini
          smid = mid * mid
