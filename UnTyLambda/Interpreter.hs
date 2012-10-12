{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)


alpha :: Term -> [Variable] -> Term
alpha (Var v) vars = (Var v)
alpha (App t t') vars = App (alpha t vars) (alpha t' vars)
alpha (Lam v b) vars = (Lam nw (alpha (subst b v (Var nw) ) (vars++[nw]) )) where
	nw = newname vars v
--- ...ddd

beta :: Term -> Term
beta (App (Lam v b) c) = subst ren v c where 
    (Lam _ ren) =  alpha (Lam v b) (free (App (Lam v b) c)) 
beta x  = x

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

hasReduct :: Term -> Bool
hasReduct (Lam v b) = hasReduct b
hasReduct (Var v) = False
hasReduct (App (Lam v b) c) = True
hasReduct (App a b) = (hasReduct a) || (hasReduct b)

hasReduct' :: Term -> Bool
hasReduct' (App (Lam v b) c) = True
hasReduct' (App a b) = (hasReduct' a) || (hasReduct' b)
hasReduct' x = False


wh, no, wa, sa :: Integer -> Term -> Term

makeReductsa :: Term -> Term
makeReductsa (Var v) = (Var v)
makeReductsa (Lam v b) = Lam v (makeReductsa b)
makeReductsa (App v1 v2) = if (hasReduct v2) then  (App v1 (makeReductsa v2)) else tmp
	where tmp = if (hasReduct v1) then (App (makeReductsa v2) v1) else (beta (App v1 v2))



-- Редукция аппликативным порядком
sa 0 t = if (hasReduct t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
sa n t = sa (n - 1) (makeReductsa t)   

makeReductno :: Term -> Term
makeReductno (Var v) = (Var v)
makeReductno (Lam v b) = Lam v (makeReductno b)
makeReductno (App (Lam v b) c) = beta (App (Lam v b) c)
makeReductno (App v1 v2) = if (hasReduct v1) then  (App (makeReductno v1) v2) else (App v1 (makeReductno v2))


-- Нормализация нормальным порядком
no 0 t = if (hasReduct t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
no n t = no (n - 1) (makeReductno t)   

makeReductwh :: Term -> Term
makeReductwh (App (Lam v b) c) = beta (App (Lam v b) c)
makeReductwh (App v1 v2) = if (hasReduct' v1) then  (App (makeReductwh v1) v2) else (App v1 (makeReductwh v2))
makeReductwh t = t

-- Редукция в слабую головную нормальную форму
wh 0 t = if (hasReduct' t) then (error $ "Too long sequence at [" ++ show t ++ "]") else t
wh n t = wh (n - 1) (makeReductwh t)   

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможн
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , (Lam "x" (Var "y")) `App` omega
    , (Lam "y" (Lam "x" ((Var "x") `App` (Var "y") `App` (Var "_x" )))) `App` (Var "x")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Hatskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле)
