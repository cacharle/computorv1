module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Numeric.Natural

import Equation


newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) input = p input

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser new_p
        where new_p s = do
                (x, s') <- p s
                return (f x, s')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser (\s -> Just (x, s))
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser new_p
        where new_p s = do
                (f, s') <- p1 s
                (x, s'') <- p2 s'
                return (f x, s'')

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\_ -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser new_p
        where new_p s = p1 s <|> p2 s


charP :: Char -> Parser Char
charP x = Parser p
    where p ""     = Nothing
          p (c:cs) = if c == x then Just (c, cs)
                               else Nothing

digitsP :: Parser String
digitsP = Parser (\s -> Just $ span isDigit s)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy x sep = ((:) <$> x <*> many (sep *> x)) <|> pure []


-- Equation parsers
-- 1 * X^0 + 2 * X^1 + 1 * 3 * X^2 = 0

coefficientP :: Parser Float
coefficientP = read <$> (floatP <|> digitsP)
    where floatP = (\i _ f -> (i ++ "." ++ f))
                        <$> digitsP <*> charP '.' <*> digitsP

exponentP :: Parser Natural
exponentP = read <$> digitsP

termP :: Parser Term
termP = (\coef _ exp -> Term coef exp)
         <$> coefficientP
         <*> (charP '*' *> charP 'X' *> charP '^')
         <*> exponentP

polynomialP :: Parser Polynomial
polynomialP = sepBy termP (charP '+')

equationP :: Parser Equation
equationP = (\l _ r -> Equation l r)
             <$> polynomialP
             <*> charP '='
             <*> polynomialP
