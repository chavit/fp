module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

if'' Nothing c d = d
if'' (Just x) c d = c

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a xs) = natOne +. length xs

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b;
(Cons a xs) ++ b = (Cons a (xs ++ b))

-- Список без первого элемента
tail :: List a -> List a
tail Nil = Nil;
tail (Cons a xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = Nil
init (Cons a Nil) = Nil
intt (Cons a xs) = (Cons a (init xs))

-- Первый элемент
head :: List a -> a
head Nil = error "!!empty list"
head (Cons a xs) = a;

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last Nil = error "!!empty list"
last (Cons a xs) = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero b = Nil
take n Nil = Nil
take n (Cons a xs) = Cons a (take (n -. natOne) xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero a = a
drop (Succ n) (Cons a  xs) = drop n xs 

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a x) = (if' (p a) (Cons a (filter p x)) (filter p x) )

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a x) = (if'' (p a) (Cons count (gfilter p x)) (gfilter p x) ) where Just count = p a 

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a x) = (if' (p a) (Cons a (takeWhile p x)) (Nil))


-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a x) = (if' (p a) (takeWhile p x) (Cons a x))

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons a x) = (if' (p a) (Pair (Cons a ((fst . span p) x)) ((snd . span p) x)) (Pair Nil x))



-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a xs) !! Zero = a
(Cons a xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse (Cons a xs) = (reverse xs) ++ (Cons a Nil)

unionAll :: List (List a) -> a -> List (List a)
unionAll Nil a = Nil
unionAll (Cons xdown xup)  a =  (Cons (Cons a xdown) (unionAll xup a))


-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a xs) = (subsequences xs) ++ (unionAll (subsequences xs) a) 

insertAll :: List a -> a -> List (List a)
insertAll Nil a = Cons (Cons a Nil) Nil
insertAll (Cons x  xs) a = Cons (Cons a (Cons x xs)) (unionAll (insertAll xs a) x)

addAll :: List (List a) -> a -> List (List a)
addAll Nil  a =Nil;
addAll (Cons a x) b = (insertAll a b) ++ (addAll x b) 

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Nil
permutations (Cons a Nil) = Cons (Cons a Nil) Nil
permutations (Cons a xs) = addAll (permutations xs) a

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = (Cons a (repeat a))

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons b x) = foldl f (f z b) x

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Nil
scanl f z (Cons b x) = Cons (f z b) (scanl f (f z b) x) 

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a x) = f a (foldr f z x) 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Nil 
scanr f z (Cons a x) = Cons (f a ((scanr f z x) !! Zero)) (scanr f z x)

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a x) =Cons (f a) (map f x)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a b) = a ++ (concat b)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap p Nil = Nil
concatMap p (Cons a b) = (p a) ++ (concatMap p b)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a Nil = Nil
zip Nil b = Nil
zip (Cons a xs) (Cons b ys) = Cons (Pair a b) (zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith p a Nil = Nil
zipWith p Nil b = Nil
zipWith p (Cons a xs) (Cons b ys) = Cons (p a b) (zipWith p xs ys)
