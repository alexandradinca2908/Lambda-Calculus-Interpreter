module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
--  TRUE: λx.λy.x
bTrue = Abs "x" $ Abs "y" vx
--  FALSE: λx.λy.y
bFalse = Abs "x" $ Abs "y" vy
--  AND: λx.λy.((x y) x)
bAnd = Abs "x" $ Abs "y" $ App (App vx vy) vx
--  OR: λx.λy.((x y) y)
bOr = Abs "x" $ Abs "y" $ App (App vx vx) vy
--  NOT: λx.((x FALSE) TRUE)
bNot = Abs "x" $ App (App vx bFalse) bTrue
--  XOR: λm.λn.((m (NOT n)) n)
bXor = Abs "m" $ Abs "n" $ App (App vm (App bNot vn)) vn

-- 4.2. Pair encodings
--  Pair: λx.λy.λz.((z x) y)
pair = Abs "x" $ Abs "y" $ Abs "z" $ App (App vz vx) vy
--  First: λx.(x TRUE)
first = Abs "x" $ App vx bTrue
--  Second: λx.(x FALSE)
second = Abs "x" $ App vx bFalse

-- 4.3. Natural number encodings
--  N0: λf.λx.x
n0 = Abs "f" $ Abs "x" vx
--  N1: λf.λx.(f x)
n1 = Abs "f" $ Abs "x" $ App vf vx
--  N2: λf.λx.(f (f x))
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)
--  Succ: λn.λf.λx.(f ((n f) x))
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App vf $ App (App vn vf) vx
--  Pred: λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λz.z)
nPred = Abs "n" $ Abs "f" $ Abs "x" $ App (App (App vn (Abs "g" $ Abs "h" $ App vh $ App vg vf)) 
    $ Abs "u" vx) (Abs "z" vz)
--  Add: λn.λm.λf.λx.((n f) ((m f) x))
nAdd = Abs "n" $ Abs "m" $ Abs "f" $ Abs "x" $ App (App vn vf) $ App (App vm vf) vx
--  Sub: λn.λm.((m pred) n)
nSub = Abs "n" $ Abs "m" $ App (App vm nPred) vn
--  Mult: λn.λm.λf.λx.((n (m f)) x)
nMult =  Abs "n" $ Abs "m" $ Abs "f" $ Abs "x" $ App (App vn (App vm vf)) vx

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    , ("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
