module Equation where
--module Equation (parse, solve) where

import Control.Applicative

data Equation = Empty | Leaf Double | Tree Operator Equation Equation
data Operator = Plus | Minus | Multiply | Divide
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

parse :: String -> Equation
parse = parseParts . parseString

parseString :: String -> Parts
parseString = filter (not . null) . foldr parse' []
    where parse' c [] = [[c]]
          parse' c xs@(x:xs')
              | c `elem` "()" = []:xs
              | otherwise     = (c:x):xs'

parseParts :: Parts -> Equation
parseParts = undefined

solve :: Equation -> Maybe Double
solve Empty          = Nothing
solve (Leaf n)       = Just n
solve (Tree o e1 e2) = f <$> solve e1 <*> solve e2
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
