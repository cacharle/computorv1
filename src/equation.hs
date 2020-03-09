module Equation where

import Numeric.Natural
import Data.List


data Equation = Equation { left :: Polynomial, right :: Polynomial }
type Polynomial = [Term]
data Term = Term { coefficient :: Float, exponent :: Natural }

instance Eq Term where
    (Term _ e1) == (Term _ e2) = e1 == e2

instance Ord Term where
    compare (Term _ e1) (Term _ e2) = compare e1 e2

instance Show Term where
    show (Term c e) = show c ++ " * X^" ++ show e

instance Show Equation where
    show (Equation l r) = showPolynomial l ++ " = " ++ showPolynomial r
        where showPolynomial [] = "0"
              showPolynomial p  = intercalate " + " (map show p)

equationMap :: (Polynomial -> Polynomial) -> Equation -> Equation
equationMap f (Equation l r) = Equation (f l) (f r)

degree :: Polynomial -> Natural
degree p = Equation.exponent (maximum p)

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
    | phi > 0   = [ (-b + sqrt phi) / (2.0 * a)
                  , (-b - sqrt phi) / (2.0 * a)
                  ]
    where phi = b ^ 2 - 4.0 * a * c

solveDegree1 :: Float -> Float -> Float
solveDegree1 b c = -c / b

solve :: Polynomial -> [Float]
solve [t0]         = []
solve [t0, t1]     = [solveDegree1 (coefficient t1) (coefficient t0)]
solve [t0, t1, t2] = solveDegree2 (coefficient t2) (coefficient t1) (coefficient t0)
solve _            = undefined
