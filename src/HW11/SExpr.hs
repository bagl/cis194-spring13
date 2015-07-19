module HW11.SExpr where

import Control.Applicative (Applicative, pure, (<$>), (<*>), liftA2, (<|>))
import Data.Char (isSpace, isAlpha, isAlphaNum)

import HW11.AParser

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 $ flip const

(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr (liftA2 (:) . f) $ pure []

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = mapA id

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = (<$>) . replicate

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = zeroOrMore space

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

ident :: Parser String
ident = (:) <$> alpha <*> zeroOrMore alphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

trimSpaces :: Parser a -> Parser a
trimSpaces p = spaces *> p <* spaces

trimParens :: Parser a -> Parser a
trimParens p = char '(' *> p <* char ')'

parseAtom :: Parser Atom
parseAtom = fmap N posInt <|> fmap I ident

parseSExpr :: Parser SExpr
parseSExpr = trimSpaces $ atom <|> comb
  where atom = A    <$> parseAtom
        comb = Comb <$> trimParens (oneOrMore parseSExpr)
