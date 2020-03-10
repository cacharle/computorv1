module Equation
( Equation (..)
, Polynomial
, Term (..)
, degree
, reduce
, solve
, filterNull
) where

import Data.List


data Equation = Equation { left :: Polynomial, right :: Polynomial }
type Polynomial = [Term]
data Term = Term { coefficient :: Float, exponent :: Int }

instance Eq Term where
    (Term _ e1) == (Term _ e2) = e1 == e2

instance Ord Term where
    compare (Term _ e1) (Term _ e2) = compare e1 e2

instance Show Term where
    show (Term 0 e) = ""
    show (Term c 0) = show (round c)
    show (Term c e) = show (round c) ++ " * X^" ++ show e

instance Show Equation where
    show (Equation l r) = showPolynomial (filterNull l)
                          ++ " = "
                          ++ showPolynomial (filterNull r)
        where showPolynomial [] = "0"
              showPolynomial p  = dropWhile (`elem` " +") $ foldl f "" (map show p)
                where f s "" = s
                      f s (c:cs)
                        | c == '-'  = s ++ " - " ++ cs
                        | otherwise = s ++ " + " ++ (c:cs)


filterNull :: Polynomial -> Polynomial
filterNull = filter (\t -> coefficient t /= 0)

equationMap :: (Polynomial -> Polynomial) -> Equation -> Equation
equationMap f (Equation l r) = Equation (f l) (f r)

degree :: Polynomial -> Int
degree [] = 0
degree p  = Equation.exponent (maximum p)

reduce :: Equation -> Equation
reduce equ = Equation (merge (left stdForm) (right stdForm)) []
    where stdForm = equationMap (\a -> (reducePolynomial $ sort a)) equ
          merge [] rs = rs
          merge ls [] = ls
          merge (l:ls) (r:rs)
            | l == r = (subTerm l r) : merge ls rs
            | l < r  = l : merge ls (r:rs)
            | r < l  = r : merge (l:ls) rs
            where subTerm (Term c1 e) (Term c2 _) = Term (c1 - c2) e
          reducePolynomial []         = []
          reducePolynomial [t]        = [t]
          reducePolynomial (t1:t2:ts)
            | t1 == t2  = (addTerm t1 t2) : reducePolynomial ts
            | otherwise = t1 : reducePolynomial (t2:ts)
            where addTerm (Term c1 e) (Term c2 _) = Term (c1 + c2) e

solveDegree2 :: Float -> Float -> Float -> [Float]
solveDegree2 a b c
    | phi < 0   = []
    | phi == 0  = [(-b) / (2.0 * a)]
    | phi > 0   = [ (-b + mySqrt phi) / (2.0 * a) -- not alowed
                  , (-b - mySqrt phi) / (2.0 * a)
                  ]
    where phi = b * b - 4.0 * a * c

solveDegree1 :: Float -> Float -> Float
solveDegree1 b c = -c / b

solve :: Polynomial -> [Float]
solve [t0]         = []
solve [t0, t1]     = [solveDegree1 (coefficient t1) (coefficient t0)]
solve [t0, t1, t2] = solveDegree2 (coefficient t2) (coefficient t1) (coefficient t0)
solve _            = undefined

mySqrt :: Float -> Float
mySqrt n
  | n < 0     = undefined
  | otherwise = mySqrt' (n / 2)
    where mySqrt' x = if abs (x * x - n) < 0.01
                         then x
                         else mySqrt' xn
            where xn = b - (a * a) / (2 * b)
                    where a = (n - x * x) / (2 * x)
                          b = x + a
