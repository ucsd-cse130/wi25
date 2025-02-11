module Lec_2_13_25  where

data List a
    = Nil
    | Cons a (List a)
    deriving (Show)

len :: List a -> Int
len l = case l of
            Nil      -> 0
            Cons _ t -> 1 + len t

bob a b = [a, b]

