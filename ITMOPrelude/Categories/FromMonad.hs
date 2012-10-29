{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish
-- делаем нас
instance Monad m => MonadFish m where
    returnFish = return 
    f >=> g = \c -> (return c) >>= f >>= g 

instance Monad m => Functor m where
	fmap f ma = ma >>= (return . f)

instance Monad m => MonadJoin m where
    returnJoin = return 
    join  = \c -> (return c) >>= id >>= id 

