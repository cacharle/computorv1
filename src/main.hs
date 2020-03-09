import System.Environment
import Data.List

import Parser
import Equation

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    equ <- checkParsing (head args)
    let reduced = reduce equ
    putStrLn $ "Reduced From: " ++ show reduced
    putSolutions (left reduced)


checkArgs :: [String] -> IO ()
checkArgs args
    | length args == 0 = fail "Usage ./computor equation"
    | length args > 1  = fail "Too many arguments"
    | otherwise        = return ()

checkParsing :: String -> IO Equation
checkParsing input = case parse Parser.equationP input
                        of Nothing        -> fail "Couldnt parse equation"
                           Just (equ, "") -> return equ
                           Just (_, s)    -> fail "Couldnt parse equation yo"

putSolutions :: Polynomial -> IO ()
putSolutions p
    | degree p > 2 = fail "The polynomial degree is strictly greater then 2, can't solve."
    | otherwise    = putStr $ intercalate "\n" (map show (solve p))
