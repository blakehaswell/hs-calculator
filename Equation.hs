module Equation (solve, sampleEq, sampleEq') where

data Equation = Leaf Double | Tree Operator Equation Equation
data Operator = Plus | Minus | Multiply | Divide

sampleEq :: Equation
sampleEq =
    Tree Plus
        (Leaf 3)
        (Tree Multiply
            (Leaf 5)
            (Leaf 2)
        )

sampleEq' :: Equation
sampleEq' =
    Tree Multiply
        (Tree Plus
            (Leaf 3)
            (Leaf 5)
        )
        (Leaf 2)

solve :: Equation -> Double
solve (Leaf n)       = n
solve (Tree o e1 e2) = f (solve e1) (solve e2)
    where f = case o of Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> (/)
