module Main where

import Equation (parse, solve)

main :: IO ()
main = putStrLn . solve' =<< getContents
    where solve' e = maybe "Parse error." (show . solve) (parse e)
