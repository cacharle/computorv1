module Parser
( parse
, equationP
) where

import Control.Applicative
import Control.Monad
import Data.Char

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

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser p
    where p []     = Nothing
          p (c:cs) = if f c then Just (c, cs)
                            else Nothing

digitsP :: Parser String
digitsP = some (satisfy isDigit) -- at least one digit to avoid read exception

spacesP :: Parser String
spacesP = many (satisfy isSpace)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy x sep = many (sep *> x)

prefixedIntP :: Parser Int
prefixedIntP = read <$> numStr
    where numStr = ((:) <$> charP '-' <*> (spacesP *> digitsP))
                    <|> (charP '+' *> spacesP *> digitsP)

intP :: Parser Int
intP = prefixedIntP <|> (read <$> digitsP)

naturalP :: Parser Int
naturalP = read <$> digitsP

floatPositiveP :: Parser Float
floatPositiveP = (f <$> digitsP <*> charP '.' <*> digitsP) <|> (read <$> digitsP)
    where f pos dot dec = read $ pos ++ [dot] ++ dec

signP :: Parser Char
signP = charP '-' <|> charP '+'

optionnal :: Parser a -> a -> Parser a
optionnal p placeholder = p <|> pure placeholder


-- Equation parsers

unsignedTermP :: Parser Term
unsignedTermP = fullP <|> varExpP <|> varConstP <|> constP
    where
        -- 1 * X ^ 1
        fullP     = (\c e -> Term c e) <$> floatPositiveP <* mulP <* varP <* expP <*> naturalP
        -- X ^ 1
        varExpP   = (\e -> Term 1 e)   <$> (varP *> expP *> naturalP)
        -- 1 * X
        varConstP = (\c -> Term c 1)   <$> floatPositiveP <* mulP <* varP
        -- 1
        constP    = (\c -> Term c 0)   <$> floatPositiveP

        mulP = spacesP *> charP '*' *> spacesP
        varP = spacesP *> charP 'X' *> spacesP
        expP = spacesP *> charP '^' *> spacesP

signedTermP :: Parser Term
signedTermP = signF <$> signP <*> (spacesP *> unsignedTermP)
    where signF '-' (Term c e) = Term (-c) e
          signF _   t          = t

firstTermP :: Parser Term
firstTermP = signedTermP <|> unsignedTermP

polynomialP :: Parser Polynomial
polynomialP = ((:) <$> firstTermP <*> (spacesP *> (sepBy signedTermP spacesP)))

equationP :: Parser Equation
equationP = (\l r -> Equation l r)
             <$> polynomialP
             <*> (spacesP *> charP '=' *> spacesP *> polynomialP)
