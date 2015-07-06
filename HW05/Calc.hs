module HW05.Calc where

import HW05.ExprT
import HW05.Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

main :: IO ()
main = do
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  print $ evalStr "(2+3)*4"
  print $ evalStr "2+3*"
  print (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
