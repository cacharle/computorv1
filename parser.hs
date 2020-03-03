module Parser where

import Control.Applicative


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser new_p
        where new_p s = do
                (x, s') <- p s
                return (f x, s')

instance Applicative Parser where
    pure x = Parser (\s -> Just (x, s))
    (Parser p1) <*> (Parser p2) = Parser new_p
        where new_p s = do
                (f, s') <- p1 s
                (x, s'') <- p2 s'
                return (f x, s'')
    -- (Parser p1) *> (Parser p2) = Parser new_p
    -- (Parser p1) <* (Parser p2) = Parser new_p

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    (Parser p1) <|> (Parser p2) = Parser new_p
        where new_p s = p1 s <|> p2 s


data Equation
    = EquationAdd
    | EquationSub
    | EquationMul
    -- | EquationDiv ?
    | EquationExp
    | EquationEqu
    | EquationNum Float

charP :: Char -> Parser Char
charP c = Parser f
    where f ""     = Nothing
          f (c:cs) = Just (c, cs)

-- equationAddP :: Parser Equation
-- equationAddP = charP '+' *> EquationAdd

-- equationP :: Parser Equation
-- equationP =
