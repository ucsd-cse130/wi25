{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Arith.Types where

import Control.Exception
import Data.Typeable

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

data Aexpr
  = AConst  Int
  | AVar    Id
  | APlus   Aexpr Aexpr
  | AMinus  Aexpr Aexpr
  | AMul    Aexpr Aexpr
  | ADiv    Aexpr Aexpr
  deriving (Eq, Show)

type Id = String

type Env = [(Id, Int)]

type Value = Int

-- $alpha stuff*

-- re1 re2


-- (x + y) * 2
expxy2 :: Aexpr
expxy2 = AMul (APlus (AVar "x") (AVar "y")) (AConst 2)

-- >>> eval [ ("y", 20)] expxy2
-- Error {errMsg = "Unbound variable x"}

-- >>> eval [] (AMinus (APlus (AConst 10) (AConst 20)) (AVar "z"))
-- Error {errMsg = "Unbound variable z"}

-- >>> (read "(7, 9, 12)") :: (Int, Int, Int)
-- (7,9,12)

eval :: Env -> Aexpr -> Value
eval _   (AConst i)     = i
eval env (AVar   x)     = case lookup x env of
                            Nothing -> errUnbound x
                            Just val -> val
eval env (APlus  e1 e2) = eval env e1 + eval env e2
eval env (AMinus e1 e2) = eval env e1 - eval env e2
eval env (AMul   e1 e2) = eval env e1 * eval env e2
eval env (ADiv   e1 e2) = eval env e1 `div` eval env e2

myLookup :: Id -> [(Id, Int)] -> Maybe Int
myLookup _ []           = Nothing
myLookup x ((k,v):rest)
  | x == k              = Just v
  | otherwise           = myLookup x rest

-- >>> lookup "cot" [("cat", 10), ("dog", 20)]
-- Nothing

-- data Maybe a = Nothing | Just a

errUnbound :: String -> a
errUnbound x = throw (Error ("Unbound variable " ++ x))

-- >>> eval [("x", 100)] (APlus (APlus (AVar "x") (AVar "y")) (AConst 10))
-- Error {errMsg = "Unbound variable y"}

-- >>> "2 + 7"

-- data Maybe      val = Nothing  | Just  val



-- data Either e a = Left e | Right a

bob :: [ Either String Int ]
bob = [ Left "thing", Right 67 ]
