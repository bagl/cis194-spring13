module HW08.Party where

import Data.List
import Data.Monoid
import Data.Tree

import HW08.Employee

-- Ex 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) $ fun + empFun e

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) $ f1 + f2

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node a []) = f a [z]
treeFold f z (Node a ts) = f a $ map (treeFold f z) ts

-- Ex 3
nextLevel :: Employee
          -> [(GuestList, GuestList)]
          -> (GuestList, GuestList)
nextLevel e ls = (glCons e with, without)
 where with    = mconcat $ map snd ls
       without = mconcat $ map (uncurry moreFun) ls

-- Ex 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel mempty

-- Ex 5
main :: IO ()
main = readFile "HW08/company.txt" >>= putStrLn . empsToGL
  where
    empsToGL :: String -> String
    empsToGL = formatGL . maxFun . parseET

    parseET :: String -> Tree Employee
    parseET = read

    formatGL :: GuestList -> String
    formatGL (GL es fun) =
      let header = "Total fun: " ++ show fun
          participants = unlines $ sort $ map empName es
      in unlines [header, participants]
