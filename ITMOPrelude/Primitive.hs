--{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)


-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

pairBoolToTri :: Bool -> Bool -> Tri
pairBoolToTri fl True = EQ
pairBoolToTri False False = GT
pairBoolToTri True False = LT
 


infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

natToBool :: Nat -> Bool
natToBool (Succ _) = True
natToBool Zero = False


-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp n m = pairBoolToTri (natLt n m) (natEq n m) 

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq n m = not (natToBool (n -. m)) && not (natToBool (m -. n)) 

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt n m = natToBool (m -. n)

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. n = Zero;
n -. Zero = n;
(Succ n) -. (Succ m) = n -. m; 

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivDown :: Tri -> Nat -> Nat -> Nat
natDivDown fl n Zero = error "!!Division by Zero"
natDivDown GT n m = Zero 
natDivDown fl n m = natOne +. natDivDown  (natCmp m (n -. m)) (n -. m) m

natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "!!Division by Zero"
natDivMod n m = Pair (natDivDown (natCmp m n) n m) ( n -. m *. (natDivDown (natCmp m n) n m))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток


natMax :: Nat -> Nat -> Nat 
natMax a b = if' (natLt a b) b a

natMin :: Nat -> Nat -> Nat 
natMin a b = if' (natLt a b) a b
 
-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (natMod a b)


-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat  deriving (Show,Read)

intZero   = Pos natZero   -- 0
intOne    = Pos natOne     -- 1
intNegOne = Neg natZero -- -1

intAbs :: Int -> Nat
intAbs (Pos a) = a;
intAbs (Neg a) = a +. natOne

intSign :: Int -> Nat
intSign (Pos a) = natZero
intSign (Neg a) = natOne

pnatToInt :: Nat -> Nat -> Int
pnatToInt Zero l = intZero
pnatToInt m Zero = Pos m;
pnatToInt m (Succ _) = Neg (m -. natOne)

-- n -> - n
intNeg :: Int -> Int
intNeg a = pnatToInt (intAbs a) (natOne -. intSign a)  


-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp n m = pairBoolToTri (intLt n m) (intEq n m)

intEq :: Int -> Int -> Bool
intEq n m  = natEq (intAbs n) (intAbs m) && natEq (intSign n) (intSign m) 

intLt :: Int -> Int -> Bool
intLt n m = natToBool (intSign (n .-. m))

intAdd :: Int -> Int -> Bool -> Int
intAdd n m True = pnatToInt ((intAbs n) +. (intAbs m)) (intSign n)
intAdd n m False = pnatToInt ((intAbs n) -. (intAbs m)) (intSign n)
 

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
n .+. m = intAdd (if' (natLt (intAbs n) (intAbs m)) m n) (if' (natLt (intAbs n) (intAbs m)) n m) (natEq (intSign n) (intSign m))

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. m =pnatToInt ((intAbs n) *. (intAbs m))  (natMod ((intSign n) +. (intSign m)) (Succ natOne))

infixl 7 .*..
(.*..) :: Int -> Nat -> Int
n .*.. m = pnatToInt ((intAbs n) *. m)  (intSign n)


-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y


-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat x y) =  Rat (pnatToInt y (intSign x)) (intAbs x) 

ratNorm :: Rat -> Rat 
ratNorm (Rat x y) =  Rat (pnatToInt (natDiv (intAbs x) (gcd (intAbs x) y)) (intSign x)) (natDiv y (gcd (intAbs x) y)) 

ratIntUp :: Rat -> Int
ratIntUp (Rat x y) = x;

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp n m= pairBoolToTri (ratLt n m) (ratEq n m) 

ratEq :: Rat -> Rat -> Bool
ratEq n m = (intEq (ratIntUp (n %- m)) (intZero))

ratLt :: Rat -> Rat -> Bool
ratLt n m= (intLt (ratIntUp (n %- m)) (intZero))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x1 y1) %+ (Rat x2 y2) = ratNorm (Rat (x1 .*.. y2 .+. x2 .*.. y1) (y1 *. y2))

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x1 y1) %* (Rat x2 y2) = ratNorm (Rat (x1 .*. x2) (y1 *. y2))

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
