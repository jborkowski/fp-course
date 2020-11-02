-- |

module Course.Tautology where

import Prelude
import Data.Maybe (isJust)
import Data.List (find)
import Course.HigherOrderFunctions (int2bin)

data Prop =
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = isJust . find ((\(k, _) -> x == k)) $  s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q


vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q



bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
  where
    range       = [0..(2^n)-1]
    make n bs   = take n (bs ++ repeat 0)
    conv 0      = False
    conv 1      = True

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
  where
    bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools' (length vs))
  where
    vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


-- | Abstract machine

data Expr = Val Int | Add Expr Expr

value' :: Expr -> Int
value' (Val n) = n
value' (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

evalExpr :: Expr -> Cont -> Int
evalExpr (Val n)   c = execOp c n
evalExpr (Add x y) c = evalExpr x (EVAL y : c)

execOp :: Cont -> Int -> Int
execOp [] n           = n
execOp (EVAL y : c) n = evalExpr y (ADD n : c)
execOp (ADD n : c)  m = execOp c (n+m)

value :: Expr -> Int
value e = evalExpr e []
