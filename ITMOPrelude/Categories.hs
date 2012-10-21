{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
import ITMOPrelude.Algebra

class Category cat where
	id :: cat a a
	(%.) :: cat b c -> cat a b -> cat a c
	
instance Category (->) where 
    id = \x -> x
    (%.)  = (.)

class Functor m where 
	fmap :: (a -> b) -> m a -> m b

instance Functor Maybe  where 
	fmap f Nothing = Nothing
	fmap f (Just a) = Just (f a) 

instance Functor (Either a) where
    fmap f (Left a) = (Left a)
    fmap f (Right b) = (Right $ f b)

instance Functor List where
	fmap  = map;

instance Functor Tree where
	fmap = mapTree

class Functor m => Monad m where 
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b

instance Monad List where 
    return a = Cons a Nil
    Nil >>= f = Nil
    (Cons x xs) >>= f = (f x) ++ (xs >>= f) 

instance Monad Maybe where
	return a = Just a	
	Nothing >>= f = Nothing
	(Just a) >>= f = f a

instance Monad (Either a) where
    return a = Right a
    (Left a) >>= f = Left a
    (Right b) >>= f = f b

class (Monad m) => (MonadFish m) where
	(>=>) :: (a -> m b )-> (b -> m c) -> (a -> m c)
	f >=> g = \c -> (return c) >>= f >>= g  

class (MonadFish m) => (MonadJoin m) where 
	join :: m (m a) -> m a
	join = (\x -> x) >=> (\x -> x)

class (MonadJoin m) => (MonadBind m) where
    bind ::  m a -> (a -> m b) -> m b	
    bind = (\x f -> join (fmap f x))
