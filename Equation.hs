module Equation where
--module Equation (parse, solve) where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Char (char, digit)
import Text.Parsec.Combinator (many1, option)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import qualified Text.Parsec.Prim as P ((<|>), parse)
import Text.Parsec.String (Parser)

data Equation = Empty | Leaf Double | Tree Op Equation Equation deriving (Show)
data Op = Plus | Minus | Multiply | Divide deriving (Show)

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
parse s = either (const Nothing) Just (P.parse parseEquation "" s)

parseEquation :: Parser Equation
parseEquation = buildExpressionParser table parseSubExpression
    where table     = [[ getOp '*' Multiply, getOp '/' Divide ],
                       [ getOp '+' Plus,     getOp '-' Minus ]]
          getOp c o = Infix (char c >> return (Tree o)) AssocLeft

parseSubExpression :: Parser Equation
parseSubExpression = char '(' *> parseEquation <* char ')'
               P.<|> parseDouble

parseDouble :: Parser Equation
parseDouble = fmap (Leaf . read) $ (++) <$> integer <*> fraction
    where integer  = many1 digit
          fraction = option "" $ (:) <$> char '.' <*> many1 digit

solve :: Equation -> Maybe Double
solve Empty          = Nothing
solve (Leaf n)       = Just n
solve (Tree o e1 e2) = f <$> solve e1 <*> solve e2
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
