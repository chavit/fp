module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List

data Tree a = Null | Build  a (Tree a) (Tree a)  deriving (Show,Read)

addRoot :: Tree a -> a -> Tree a
addRoot b a = Build a b Null

addLeft :: Tree a -> a -> Tree a
addLeft Null  a = Build a Null Null
addLeft (Build c x y) a = Build c (addLeft x a) y

addRight :: Tree a -> a -> Tree a
addRight Null a = Build a Null Null
addRight (Build c x y) a = Build c x (addRight y a)

turnLeft :: Tree a -> Tree a
turnLeft Null = Null
turnLeft (Build a Null b) = (Build a Null b)
turnLeft (Build rt (Build a x y) b) = Build a x (Build rt y b)

turnRight :: Tree a -> Tree a
turnRight Null = Null
turnRight (Build a b Null) = (Build a b Null)
turnRight (Build rt a (Build b x y)) = Build b (Build rt a x) y

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Null = Null
mapTree f (Build a b c) = Build (f a) (mapTree f b) (mapTree f c)

foldTree :: (a -> b -> b -> b) -> b ->Tree a -> b
foldTree f z Null = z
foldTree f z (Build q w r) = (f q (foldTree f z w) (foldTree f z r))
