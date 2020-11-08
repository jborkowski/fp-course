{-# LANGUAGE OverloadedStrings #-}
-- | The countdown problem
{-
 Given a sequence of numbers and a target number, attempt to construct an expression whose
 value is the target, by combining one or more numbers from the sequence using addition, subtraction,
 multiplication, division and parantheses
-}

module Course.CountDown where

import Course.Core
import qualified Prelude as P

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y -- removes duplicates
valid Div x y = y /= 1 && x `mod` y == 0   -- removes duplicates

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l P.++ show o P.++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" P.++ show e P.++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l P.++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y ]

subs :: [a] -> [[a]]
subs []         = [[]]
subs (x:xs)     = yss P.++ P.map (x:) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []         = [[x]]
interleave x (y:ys)     = (x:y:ys) : P.map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms = P.foldr (P.concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices = P.concat . P.map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  P.elem (values e) (choices ns) && eval e == [n]

-- | Brutal Force
split :: [a] -> [([a],[a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []        = []
exprs [n]       = [Val n]
exprs ns        = [e | (ls,rs) <- split ns
                     , l <- exprs ls
                     , r <- exprs rs
                     , e <- combine l r]
                  where
                    combine :: Expr -> Expr -> [Expr]
                    combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

brutalForceSolution :: [Int] -> Int -> [Expr]
brutalForceSolution ns n =
  [e | ns' <- choices ns
     , e <- exprs ns'
     , eval e == [n]]


type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns
                  , lx <- results ls
                  , ry <- results rs
                  , res <- combine lx ry]
             where
               combine :: Result -> Result -> [Result]
               combine (l,x) (r,y) =
                 [(App o l r, apply o x y) | o <- ops, valid o x y]

-- | 100 times faster than brutalForceSolution
-- | Removes all evaluation that fails at ealier stage
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns
     , (e,m) <- results ns'
     , m == n]





main :: IO ()
main = P.print (solutions [1,3,7,10,25,50] 765)
