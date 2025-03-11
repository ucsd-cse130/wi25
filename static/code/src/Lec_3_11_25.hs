{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# LANGUAGE DeriveFunctor #-}
module Lec_3_11_25 where






data Tree a = Node (Tree a) (Tree a) | Leaf a
    deriving (Show)

tree0 :: Tree Int
tree0 = Node (Node
                (Leaf 1)
                (Leaf 2)
             )
             (Node
                (Leaf 3)
                (Leaf 4)
             )
data Result e v
  = Err e
  | Val v
  deriving (Show)


{-

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

-}

instance Functor (Result e) where
instance Applicative (Result e) where

instance Monad (Result e) where
    (>>=) = pat
    return = ret

pat :: Result e t -> (t -> Result e v) -> Result e v
pat thing doStuff =
    case thing of
        Err e -> Err e
        Val v -> doStuff v

ret :: a -> Result e a
ret = Val
