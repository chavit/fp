module Monstupar.Derived where

-- В этом модуле не видно структуры типа Monstupar из
import Monstupar.Core
-- например,
-- blah = Monstupar $ undefined
-- не скомпилируется, поскольку конструктор Monstupar приватный,
-- поэтому конструировать парсеры тут можно только используя примитивные
-- парсеры из Core.

--------------------------------------------------------------------------------
-- Всякие удобные и полезные штуки

-- Всё плохо
notok :: Monstupar s ()
notok = isnot ok

-- В голове ввода сейчас в точности s
char :: Eq s => s -> Monstupar s s
char = like . (==)

-- В голове ввода сейчас что-то из списка
oneOf :: Eq s => [s] -> Monstupar s s
oneOf [] = like (\_ -> False) 
oneOf (x:xs) = (char x) <|> (oneOf xs) 

-- В префиксе головы сейчас нечто вполне определённое
string :: Eq s => [s] -> Monstupar s [s]
string [] = return []
string (x:xs) = char x >>= (\y -> (string xs >>= \ys -> return (y:ys)))
-- do
	-- y <- char x
	-- ys <- string xs
	-- return $ y:ys

-- "Звёздочка" -- запустить парсер максимальное (ноль или более) число раз и
-- саккумулировать результыты
many :: Monstupar s a -> Monstupar s [a]
many p = (do
	y <- p
	ys <- many p	
	return (y:ys)) <|> return []
-- Аккуратно с реализацией! Следите за тем, чтобы у вас из-за использования <|>
-- не рос в бесконечность стек.

-- "Плюсик" -- один или более раз
many1 :: Monstupar s a -> Monstupar s [a]
many1 p = do
    e <- p 
    es <- many p
    return (e:es) 

-- "Вопросик" -- ноль или один раз
optional :: Monstupar s a -> Monstupar s (Maybe a)
optional p = (p >>= (\y -> return  (Just y)) ) <|> return Nothing 
