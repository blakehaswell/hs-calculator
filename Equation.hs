module Equation where
--module Equation (parse, solve) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char (digitToInt, isDigit)
import Debug.Trace

import Text.Parsec.Char (anyChar, char, digit)
import Text.Parsec.Combinator (many1, option)
import Text.Parsec.Expr (Assoc (..), Operator (..))
import Text.Parsec.Prim (Parsec, many)
import Text.Parsec.String (Parser)

data Equation = Empty | Leaf Double | Tree Op Equation Equation deriving (Show)
data Op = Plus | Minus | Multiply | Divide deriving (Show)
type Parts = [String]

-- 3 + 5 * 2
sampleEq :: Equation
sampleEq =
    Tree Plus
        (Leaf 3)
        (Tree Multiply
            (Leaf 5)
            (Leaf 2)
        )

-- (3 + 5) * 2
sampleEq' :: Equation
sampleEq' =
    Tree Multiply
        (Tree Plus
            (Leaf 3)
            (Leaf 5)
        )
        (Leaf 2)

parse :: String -> Maybe Equation
parse = parseParts . parseString

parseString :: String -> Parts
parseString = filter (not . null) . foldr parse' []
    where
        parse' c xs
            | c `elem` "()" = []:xs
            | otherwise     = if (not . null) xs
                                then (c:head xs):tail xs
                                else [[c]]

parseParts :: Parts -> Maybe Equation
parseParts = foldr parse' (Just Empty)
    where parse' p e = join $ joinEqs <$> parsePart p <*> e

parsePart :: String -> Maybe Equation
parsePart = foldr parse' (Just Empty)
    where parse' c e = join $ joinEqsWithPrecendence <$> parseChar c <*> e

parseChar :: Char -> Maybe Equation
parseChar c
    | isDigit c    = Just (Leaf ((fromIntegral . digitToInt) c))
    | isOperator c = Just (Tree (operatorFromChar c) Empty Empty)
    | otherwise    = Nothing
        where isOperator = (`elem` "+-*/")
              operatorFromChar '+' =  Plus
              operatorFromChar '-' =  Minus
              operatorFromChar '*' =  Multiply
              operatorFromChar '/' =  Divide

joinEqs :: Equation -> Equation -> Maybe Equation
joinEqs Empty e1             = Just e1
joinEqs e1 Empty             = Just e1
joinEqs (Tree o e1 Empty) e2 = Just (Tree o e1 e2)
joinEqs (Tree o Empty e1) e2 = Just (Tree o e1 e2)
joinEqs e1 (Tree o Empty e2) = Just (Tree o e1 e2)
joinEqs e1 (Tree o e2 Empty) = Just (Tree o e1 e2)
joinEqs (Leaf a) (Leaf b)    = Just (Leaf ((a * 10) + b))
joinEqs e1 e2                = traceShow (e1, e2) $ Nothing

joinEqsWithPrecendence :: Equation -> Equation -> Maybe Equation
joinEqsWithPrecendence (Tree Multiply e1 Empty) (Tree o e2 e3) = Just (Tree o (Tree Multiply e1 e2) e3)
joinEqsWithPrecendence (Tree Multiply Empty e1) (Tree o e2 e3) = Just (Tree o (Tree Multiply e1 e2) e3)
joinEqsWithPrecendence (Tree o e1 e2) (Tree Multiply Empty e3) = Just (Tree o (Tree Multiply e2 e3) e1)
joinEqsWithPrecendence (Tree o e1 e2) (Tree Multiply e3 Empty) = Just (Tree o (Tree Multiply e2 e3) e1)
joinEqsWithPrecendence (Tree Divide e1 Empty) (Tree o e2 e3) = Just (Tree o (Tree Divide e1 e2) e3)
joinEqsWithPrecendence (Tree Divide Empty e1) (Tree o e2 e3) = Just (Tree o (Tree Divide e1 e2) e3)
joinEqsWithPrecendence (Tree o e1 e2) (Tree Divide Empty e3) = Just (Tree o (Tree Divide e2 e3) e1)
joinEqsWithPrecendence (Tree o e1 e2) (Tree Divide e3 Empty) = Just (Tree o (Tree Divide e2 e3) e1)
joinEqsWithPrecendence e1 e2 = joinEqs e1 e2

--parsecParse :: Parser Equation
--parsecParse = do
--    d <- many digit
--    return $ Leaf (read d)

parseDouble :: Parser Equation
--parseDouble = do
--    i <- many1 digit
--    f <- option "" $ (:) <$> (char '.') <*> many1 digit
--    --f <- many digit
--    return $ Leaf (read (i ++ f))
parseDouble =
    Leaf <$> (read <$> ((++) <$> integer <*> fraction))
        where integer = many1 digit
              fraction = option "" $ (:) <$> (char '.') <*> many1 digit

--multiply = char '*'
--multiply = Infix ((char '*') >> return (Tree Multiply)) AssocLeft

solve :: Equation -> Maybe Double
solve Empty          = Nothing
solve (Leaf n)       = Just n
solve (Tree o e1 e2) = f <$> solve e1 <*> solve e2
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
