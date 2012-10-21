{-# LANGUAGE NoImplicitPrelude,FlexibleInstances #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive
import ITMOPrelude.List

class Monoid a where
   mempty :: a
   mappend :: a -> a -> a

instance Monoid Unit where
   mempty = Unit
   mappend = (\_ _ -> Unit)
   
instance Monoid (Maybe a) where
   mempty = Nothing
   mappend (Just a) x = (Just a)
   mappend Nothing x = x

newtype All = All { getAll :: Bool } 
newtype Any = Any { getAny :: Bool }

instance Monoid Any where
   mempty = Any False
   mappend x y = Any $ getAny x || getAny y	

instance Monoid All where
   mempty = All False
   mappend x y = All $ getAll x && getAll y	
  
instance Monoid Tri where 
   mempty = EQ
   mappend EQ x = x
   mappend x _ = x
   
newtype Sum a = Sum { getSum :: a }

instance Monoid (Sum Nat) where
   mempty = Sum natZero
   mappend x y = Sum $ getSum x +. getSum y

instance Monoid (Sum Int) where
   mempty = Sum intZero
   mappend x y = Sum $ getSum x .+. getSum y

instance Monoid (Sum Rat) where
   mempty = Sum ratZero
   mappend x y = Sum $ getSum x %+ getSum y

newtype Product a = Product { getProduct :: a }

instance Monoid (Product Nat) where
   mempty = Product natOne
   mappend x y = Product $ getProduct x +. getProduct y

instance Monoid (Product Int) where
   mempty = Product intOne
   mappend x y = Product $ getProduct x .+. getProduct y

instance Monoid (Product Rat) where
   mempty = Product ratOne
   mappend x y = Product $ getProduct x %+ getProduct y

instance Monoid (List a) where
   mempty = Nil
   mappend = (++)
   
class (Monoid a) => (Group a) where
   inv :: a -> a 

instance Group Unit where
   inv x = Unit 

instance Group (Sum Int) where
   inv x = Sum $ intNeg (getSum x)

instance Group (Sum Rat) where
   inv x = Sum $ ratNeg (getSum x)
