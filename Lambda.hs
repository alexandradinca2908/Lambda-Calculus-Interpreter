module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
--  Split the lambda until we can evaluate each variable independently
--  After collecting all names, remove duplicates using nub
vars :: Lambda -> [String]
vars lambda = nub $ extractAll lambda
                where
                  extractAll :: Lambda -> [String]
                  extractAll lambda = case lambda of
                    Var x -> [x]
                    App e1 e2 -> extractAll e1 ++ extractAll e2
                    Abs x e -> [x] ++ extractAll e

-- 1.2.
-- Depending on the given lambda, execute a different action:
--      for var and macro, check the bounded variables names to see if it's bounded
--      for app, evaluate both the expressions E1 and E2 individually
--      for abs, add X to bounded vars and evaluate expression E
freeVars :: Lambda -> [String]
freeVars lambda = nub $ extractFree lambda []
                    where
                        extractFree :: Lambda -> [String] -> [String]
                        extractFree lambda bounded = 
                            case lambda of
                                (Var x) -> if (elem x bounded) then []
                                            else [x]
                                (Abs x expr) -> extractFree expr ([x] ++ bounded)
                                (App expr1 expr2) -> (extractFree expr1 bounded) ++ (extractFree expr2 bounded)

-- 1.3.
--  AUXILIARY FUNCIONS

--  Generate next possible string of the given string
--  If the string contains only z's, reset to a string of a's with size + 1
--  Else, increase the next available char
generateString :: Int -> String -> String
generateString len str = case str of
    "" -> "a"
    _ -> if (last str == 'z') then generateString len (init str) ++ "a"
        else init str ++ [succ $ last str]

--  Continuously search for elements in lexicographic order until one is not found
findElem :: [String] -> String -> String
findElem strList nextString
    | (elem nextString strList) = findElem strList nextStr
    | otherwise = nextString
    where
        nextStr = generateString (length nextString) nextString

--  MAIN FUNCTION
newVar :: [String] -> String
newVar strList = case strList of
    [] -> ""
    _ -> findElem strList "a"

-- 1.4.
isNormalForm :: Lambda -> Bool
--  A variable is in normal form
isNormalForm (Var _) = True
--  An application between and abs and anything else can always be reduced
--  Therefore it isn't in normal form
isNormalForm (App (Abs _ _) _) = False
--  In abs, only evaluate the expression
isNormalForm (Abs _ e) = isNormalForm e
--  An application can be in normal form, if its expressions are in normal form
--  For example, if e1 and e2 are vars
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
--  Macros can't be in normal form
isNormalForm (Macro _) = False

-- 1.5.
--  AUXILIARY FUNCTIONS

--  (λy.e1 e2) -> rename all y vars to a new name
--  the new name must be unique in both e1 and e2
renameCollision :: Lambda -> Lambda -> Lambda
renameCollision (Abs x e1) e2 =
    Abs newVarName $ renameVar x newVarName e1
        where
            newVarName = newVar $ vars (App e1 e2)

--  Renames a variable or more depending on the given lambda
renameVar :: String -> String -> Lambda -> Lambda
--  Var: Changes variable "old" to "new"
renameVar old new (Var x) 
    | x == old  = Var new
    | otherwise = Var x
--  App: Changes the names of all "old" variables to "new" in both expressions
renameVar old new (App e1 e2) = 
    App (renameVar old new e1) (renameVar old new e2)
--  Abs: replaces all "old" variables to "new"
renameVar old new (Abs x e)
    | x == old = Abs new (renameVar old new e)
    | otherwise = Abs x (renameVar old new e)

reduce :: String -> Lambda -> Lambda -> Lambda
--  (λx.x e) -> replace x with e
--  (λx.y e) -> nothing to replace
reduce x (Var y) e
    | x == y = e
    | otherwise = Var y
--  Reduce x to e3 in both expressions (e1, e2) separately
reduce x (App e1 e2) e3 =
    App (reduce x e1 e3) (reduce x e2 e3)
reduce x (Abs y e1) e2
    --  (λx.λx.e1 e2) -> we don't replace the x in e1 due to name collision
    | x == y = Abs y e1
    --  (λx.λy.e1 e2) -> if we have y in both e1 and e2, we must rename y in e1
    | elem y $ freeVars e2 = Abs newY (reduce x newE1 e2)
    --  if there are no name collisions, just reduce x to e2
    | otherwise = Abs y (reduce x e1 e2)
        where
            Abs newY newE1 = renameCollision (Abs y e1) e2

-- 1.6.
normalStep :: Lambda -> Lambda
--  APP
--  (λx.e1 e2) -> replace x with e2 in e1
--  (e1 e2) -> reduce the first expr not in normal form
normalStep (App e1 e2) = case e1 of
    (Abs x e) -> reduce x e e2
    _         -> if (isNormalForm e1) then App e1 (normalStep e2)
                else App (normalStep e1) e2
--  ABS
--  Reduce the expression, if needed
normalStep (Abs x e)
    | isNormalForm e = Abs x e
    | otherwise = Abs x (normalStep e)
--  VAR
normalStep (Var x) = (Var x)

-- 1.7.
applicativeStep :: Lambda -> Lambda
--  APP (Abs expr)
--  (λx.e1 e2) ->  if possible, reduce within e1
--                 if not, reduce within e2
--                 if you cant reduce either, replace x with e2 in e1
applicativeStep (App (Abs x e1) e2)
    | not (isNormalForm (Abs x e1)) = App (applicativeStep (Abs x e1)) e2
    | not (isNormalForm e2) = App (Abs x e1) (applicativeStep e2)
    | otherwise = reduce x e1 e2
--  APP (expr1 expr2)
--  (e1 e2) ->  if possible, reduce within e1
--              if not, reduce within e2
applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | otherwise             = App e1 (applicativeStep e2)
--  ABS
--  Can only reduce e, if it is not in normal form
applicativeStep (Abs x e)
    | isNormalForm e = Abs x e
    | otherwise      = Abs x (applicativeStep e)
--  VAR
applicativeStep var = var

-- 1.8.
--  Applies a given step on a given lambda, returning all steps
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step lambda = [lambda] ++ if (isNormalForm lambda) then []
                                else simplify step (step lambda)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
