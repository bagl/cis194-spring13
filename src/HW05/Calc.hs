{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HW05.Calc where

import           Control.Applicative ((<$>), (<*>))
import qualified Data.Map as M
import           HW05.ExprT
import           HW05.Parser
import qualified HW05.StackVM as VM

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

instance Expr VM.Program where
  lit i   = [VM.PushI i]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i   = \_ -> Just i
  add a b = \m -> (+) <$> a m <*> b m
  mul a b = \m -> (*) <$> a m <*> b m

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = \m -> M.lookup s m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vars expr = expr $ M.fromList vars

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

main :: IO ()
main = do
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  print (testExp :: Maybe Integer)
  print (testExp :: Maybe Bool)
  print (testExp :: Maybe MinMax)
  print (testExp :: Maybe Mod7)
  print (testExp :: Maybe VM.Program)
  print $ VM.stackVM <$> compile "(3 * -4) + 5"
