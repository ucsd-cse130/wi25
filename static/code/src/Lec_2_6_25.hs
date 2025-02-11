module Lec_2_6_25  where

data Nat
    = Null
    | Ink Nat
    deriving (Show)

n0 :: Nat
n0 = Null

n1 :: Nat
n1 = Ink Null

n2 :: Nat
n2 = Ink ( Ink Null)

n5 :: Nat
n5 = Ink ( Ink ( Ink ( Ink ( Ink Null))))

-- >>> toInt n5
-- 5

toInt :: Nat -> Int
toInt n = case n of
            Null -> 0
            Ink n' -> 1 + toInt n'

toInt' :: Nat -> Int
toInt' Null     = 0
toInt' (Ink n') = 1 + toInt n'

toNat :: Int -> Nat
toNat n = if n <= 0 then Null else Ink (toNat (n-1))

-- >>> foo 20
-- Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink (Ink Null)))))))))))))))))))

-- foo 2
-- Ink (foo 1)
-- Ink (Ink (foo 0))
-- Ink (Ink ( Null ))


-- (a) Null     (b) 0       (c) ?


-- toInt :: Nat -> Int
-- toInt n =


addAsInt :: Nat -> Nat -> Nat
addAsInt n m = toNat (toInt n + toInt m)

-- >>> add (Ink (Ink Null)) (Ink (Ink (Ink Null)))
-- Ink (Ink (Ink (Ink (Ink Null))))

{-
add (Ink (Ink Null)) (Ink (Ink (Ink Null)))
==>
add ((Ink Null)) (Ink (Ink (Ink (Ink Null))))
==>
add ((Null)) (Ink (Ink (Ink (Ink (Ink Null)))))
==>
(Ink (Ink (Ink (Ink (Ink Null)))))
-}

add1 :: Nat -> Nat -> Nat
add1 Null    m = m
add1 (Ink n) m = add1 n (Ink m)

add2 :: Nat -> Nat -> Nat
add2 Null    m = m
add2 (Ink n) m = Ink (add2 n m)

-- sub :: Nat -> Nat -> Nat
sub :: Nat -> Nat -> Nat
sub n    Null       = n
sub Null _          = Null
sub (Ink n) (Ink m) = sub n m


data Expr
    = ENum Double
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr

-- >>> ( 4.0 + 2.9 ) * (3.78 - 5.92)
-- -14.766000000000002

e0 :: Expr
e0 = EMul
        (EAdd (ENum 4.0) (ENum 2.9))
        (ESub (ENum 3.78) (ENum 5.92))

-- >>> eval e0
-- -14.766000000000002

eval :: Expr -> Double
eval e = case e of
            ENum n -> n
            EAdd e1 e2 -> eval e1 + eval e2
            ESub e1 e2 -> eval e1 - eval e2
            EMul e1 e2 -> eval e1 * eval e2


data List a
    = Nil
    | Cons a (List a)
    deriving (Show)

-- [1,2,3]        => (Cons 1 (Cons 2(Cons 3 Nil)))

-- >>> len (Cons "cat" (Cons "dog" (Cons "mouse" (Cons "zebra" Nil))))
-- 4

-- >>> append (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 10 (Cons 20 (Cons 30 Nil)))

-- (Cons 1 (Cons 2 (Cons 3 (Cons 10 (Cons 20 (Cons 30 Nil))))))

{-
append (Cons 1 ) (Cons 10 (Cons 20 (Cons 30 Nil)))
==>
Cons 1 (append (Cons 2 (Cons 3 Nil)) (Cons 10 (Cons 20 (Cons 30 Nil))) )
==>
Cons 1 ((Cons 2 (append (Cons 3 Nil)) (Cons 10 (Cons 20 (Cons 30 Nil))) )
==>
Cons 1 ((Cons 2 ((Cons 3 (append Nil)) (Cons 10 (Cons 20 (Cons 30 Nil))) )
==>
Cons 1 ((Cons 2 ((Cons 3 (Cons 10 (Cons 20 (Cons 30 Nil)))

-}
append :: List a -> List a -> List a
append Nil          l2 = l2
append (Cons h1 t1) l2 = Cons h1 (append t1 l2)


-- >>> [1,2,3] ++ [10,20,30]
-- [1,2,3,10,20,30]
--

len :: List a -> Int
len l = case l of
            Nil      -> 0
            Cons _ t -> 1 + len t

bob a b = [a, b]

-- >>> 10 `bob` 20
-- [10,20]

-- add n       Null = n



--  = Ink (Ink Null)

-- (c) Ink Null
-- (e) loops forever...






-- (a) Ink (Ink Null)
-- (b) Null
-- (d) Ink (Ink (Ink Null))


-- add Null Null               = Null
-- add Null (Ink Null)         = Ink Null
-- add (Ink Null) (Ink Null)   = Ink (Ink Null)

-- add :: Nat -> Nat -> Nat
