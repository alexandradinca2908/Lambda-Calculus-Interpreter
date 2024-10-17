module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
extendLambda :: Context -> Lambda -> Either String Lambda
--  Var: nothing to replace
extendLambda ctx (Var x) = Right(Var x)
--  Abs: check expression
extendLambda ctx (Abs x e) =
    case (extendLambda ctx e) of
        Left err -> Left err
        Right lambda -> Right (Abs x lambda)
--  App: check both expressions
extendLambda ctx (App e1 e2) = 
    case (extendLambda ctx e1) of
        Left err -> Left err
        Right lambda1 -> 
            case (extendLambda ctx e2) of
                Left err -> Left err
                Right lambda2 -> Right(App lambda1 lambda2)
--  Macro: replace, if possible
extendLambda ctx (Macro x) =
    case (lookup x ctx) of
        Nothing -> Left("err")
        Just(lambda) -> Right(lambda)

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step lambda =
    case (extendLambda ctx lambda) of
        Left err -> Left err
        Right extendedLambda -> Right(simplify step extendedLambda)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
