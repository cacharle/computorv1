import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.List

import Parser
import Equation

main :: IO ()
main = catchIOError tryMain handler
    where handler e
            | isUserError e = putStrLn $ trimUserError (show e)
            | otherwise     = putStrLn "Error"
            where trimUserError s = init $ tail $ dropWhile (/='(') s

tryMain :: IO ()
tryMain = do
    args <- getArgs
    checkArgs args
    equ <- checkParsing (head args)
    let reduced = reduce equ
        l       = filterNull $ left reduced
    putStrLn $ "Reduced From: " ++ show reduced
    putStrLn $ "Polynomial degree: " ++ (show $ degree l)
    putSolutions l

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
putSolutions []  = putStrLn "Infinite solutions" -- 0 = 0
putSolutions [_] = putStrLn "No solution"        -- c = 0
putSolutions p   = case degree p of
    1 -> putStrLn $ "The solution is:\n" ++ show (head solutions)
    2 -> do case length solutions of 0 -> putStrLn "Discriminant is strictly negative, there is no solution."
                                     1 -> putStrLn "Discriminant is equal to 0, the solution is:"
                                     2 -> putStrLn "Discriminant is strictly positive, the two solutions are:"
            putStr $ intercalate "\n" $ map show solutions
    _ -> fail "The polynomial degree is strictly greater then 2, can't solve."
    where solutions = sort $ solve p
