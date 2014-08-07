module Equation where
--module Equation (parse, solve) where

data Equation = Leaf Double | Tree Operator Equation Equation

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

parseString :: String -> [String]
parseString = filter (not . null) . foldr parse' []
    where parse' c [] = [[c]]
          parse' c xs@(x:xs')
              | c `elem` "()" = []:xs
              | otherwise     = (c:x):xs'

parseParts :: [String] -> Equation
parseParts = undefined

solve :: Equation -> Double
solve (Leaf n)       = n
solve (Tree o e1 e2) = f (solve e1) (solve e2)
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
