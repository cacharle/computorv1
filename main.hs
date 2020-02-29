import System.Environment
import Data.Char

data TokenType = Number | Add | Sub | Mul | Exp | Equal deriving (Show)
data Token = Token TokenType Float deriving (Show)

main = do
    args <- getArgs
    -- putStr $ show args
    let l = lexer $ (head args)
    putStrLn $ show l


lexer :: String -> [Token]
lexer "" = []
lexer (c:rest)
    | c == ' ' = lexer rest
    | isDigit c = (Token Number (read (isolateFloat (c:rest)) :: Float)) : lexer (afterFloat (c:rest))
    | c == '+' = (Token Add 0.0) : lexer rest
    | c == '-' = (Token Sub 0.0) : lexer rest
    | c == '*' = (Token Mul 0.0) : lexer rest
    | c == '^' = (Token Exp 0.0) : lexer rest
    | c == '=' = (Token Equal 0.0) : lexer rest

    where isolateFloat :: String -> String
          isolateFloat "" = ""
          isolateFloat (c:cs)
            | isDigit c = c : isolateFloat cs
            | c == '.' = c : isolateFloat cs
            | otherwise = ""

          afterFloat :: String -> String
          afterFloat "" = ""
          afterFloat (c:cs)
            | isDigit c = afterFloat cs
            | c == '.' = afterFloat cs
            | otherwise = (c:cs)


-- parse :: Lexing -> SyntaxTree
-- parse s = 2
--
--
-- eval :: SyntaxTree -> Maybe [Float]
-- eval _ = 0.0
