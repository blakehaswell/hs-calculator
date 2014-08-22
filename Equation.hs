module Equation where
--module Equation (parse, solve) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Char (char, digit, spaces)
import Text.Parsec.Combinator (many1, option)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import qualified Text.Parsec.Prim as P ((<|>), parse, try)
import Text.Parsec.String (Parser)

data Equation = Leaf Double | Tree Op Equation Equation deriving (Show)
data Op = Plus | Minus | Multiply | Divide deriving (Show)

parse :: String -> Maybe Equation
parse s = either (const Nothing) Just (P.parse parseEquation "" s)

parseEquation :: Parser Equation
parseEquation = buildExpressionParser table parseSubExpression
    where table     = [[ getOp '*' Multiply, getOp '/' Divide ],
                       [ getOp '+' Plus,     getOp '-' Minus ]]
          getOp c o = Infix (P.try (op c) >> return (Tree o)) AssocLeft
          op c      = spaces *> char c <* spaces

parseSubExpression :: Parser Equation
parseSubExpression = char '(' *> parseEquation <* char ')'
               P.<|> parseDouble

parseDouble :: Parser Equation
parseDouble = fmap (Leaf . read) $ (++) <$> integer <*> fraction
    where integer  = many1 digit
          fraction = option "" $ (:) <$> char '.' <*> many1 digit

solve :: Equation -> Double
solve (Leaf n)       = n
solve (Tree o e1 e2) = f (solve e1) (solve e2)
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
