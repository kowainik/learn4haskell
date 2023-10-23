{-# LANGUAGE InstanceSigs #-}
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes x y = fmap (+) x <*> y

data Secret e a = Trap e | Reward a deriving Show

instance Functor (Secret e) where
  fmap :: (a -> b) -> Secret e a -> Secret e b
  fmap _ (Trap e) = Trap e
  fmap f (Reward a) = Reward $ f a

instance Applicative (Secret e) where
  pure :: a -> Secret e a
  pure = Reward
  Trap e <*> _ = Trap e
  Reward f <*> a = fmap f a
