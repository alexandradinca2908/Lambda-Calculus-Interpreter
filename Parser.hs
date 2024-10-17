module Parser (parseLambda, parseLine) where

import Data.Char

import Control.Monad
import Control.Applicative

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
--  Fail Parser (from course)
--  Returns Nothing
failParser :: Parser a 
failParser = Parser $ \s -> Nothing

--  Char Parser (from course)
--  Success: Parses and returns Just((given char, rest of the list)
--  Fail (empty list or char doesn't match): Returns Nothing
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> 
                case s of 
                    [] -> Nothing
                    (x:xs) -> if x == c then Just((x,xs)) else Nothing

--  Predicate parser (from course)
--  Success (next char matches the predicate): Returns Just((first char, rest of the list))
--  Fail (empty list or char fails the predicate): Returns Nothing
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
                       case s of 
                        [] -> Nothing
                        (x:xs) -> if p x then Just((x,xs)) else Nothing

--  Monad Parser (from course)
instance Monad Parser where
    return v = Parser $ \s -> Just((v,s))
    mp >>= f = Parser $ \s -> case parse mp s of 
                                Nothing -> Nothing
                                Just((v,rest)) -> parse (f v) rest      

--  Applicative Parser (from course)
instance Applicative Parser where
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v
  pure = return

--  Functor Parser (from course)
instance Functor Parser where 
  fmap f mp = 
    do 
      v <- mp
      return $ f v

--  Alternative Parser (from course)
instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s 
                                Just x -> Just x

--  Plus Parser (from course)
plusParser :: Parser a -> Parser [a]
plusParser p = do
                x <- p
                xs <- starParser p
                return (x:xs)

--  Star Parser (from course)
starParser :: Parser a -> Parser [a]
starParser p = (plusParser p) <|> (return [])

--  Whitespace Parser (from course)
whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

--  STRING PARSERS

--  Var Parser
--  Parses for as many letters as possible (at least one) until it finds a full Var
varParser :: Parser String
varParser = do
            xs <- plusParser (predicateParser isAlpha)
            return xs

--  Macro Parser
--  Parses as many capital letters or digits as possible (at least one) until it finds
--  a full Macro
macroParser :: Parser String
macroParser = do
            xs <- plusParser (predicateParser isUpper <|> predicateParser isDigit)
            return xs

--  LAMBDA PARSERS

--  Var Lambda Parser
--  Maps found String variables to Lambda Variables
varLambdaParser :: Parser Lambda
varLambdaParser = Var <$> varParser

--  Var Macro Parser
--  Maps found String macros to Lambda Macros
macroLambdaParser :: Parser Lambda
macroLambdaParser = Macro <$> macroParser

--  Abs Lambda Parser
--  Parses the following sequence: "\x.expr"
absLambdaParser :: Parser Lambda
absLambdaParser = do
                _ <- charParser '\\'
                val <- varParser
                _ <- charParser '.'
                expr <- atomParser
                return (Abs val expr)

--  App Lambda Parser
--  Parses the following sequence: "(expr1 expr2)"
appLambdaParser :: Parser Lambda
appLambdaParser = do
                  _ <- (charParser '(')
                  expr1 <- atomParser
                  _ <- whitespaceParser
                  expr2 <- atomParser
                  _ <- (charParser ')')
                  return (App expr1 expr2)

--  Atom Parser
atomParser :: Parser Lambda
atomParser = macroLambdaParser <|> varLambdaParser 
    <|> absLambdaParser <|> appLambdaParser

--  Main Parser
parseLambda :: String -> Lambda
parseLambda str = case (parse atomParser str) of
    Just(x, "") -> x
    _ -> (Var "")

-- 3.3.
--  Eval parser
--  Parses any atom and wraps in Eval container
evalParser :: Parser Line
evalParser = do 
    atom <- atomParser
    return (Eval atom)

--  Binding parser
--  Parses the following sequence: "MACRO=atom" and wraps in Binding container
bindingParser :: Parser Line
bindingParser = do
    macro <- macroParser
    _ <- charParser '='
    atom <- atomParser
    return (Binding macro atom)

--  Line parser
lineParser :: Parser Line
lineParser = bindingParser <|> evalParser

--  Main line parser
parseLine :: String -> Either String Line
parseLine str = case (parse lineParser str) of
    Just(x, "") -> Right x
    _ -> Left ""
