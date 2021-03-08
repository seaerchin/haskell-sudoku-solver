module Main where

import Constants
import Control.Monad
import Sudoku

main :: IO ()
main =
  driver
    >>= \puzzle ->
      print puzzle
        >> print
          (sudoku puzzle)

driver :: IO [String]
driver = forM [1 :: Int .. 9] (const getLine)

solveEasy :: IO ()
solveEasy = mapM_ putStrLn (format diabolical) >> mapM_ putStrLn ((prettyPrint . head . sudoku) diabolical)

prettyPrint :: [[String]] -> [String]
prettyPrint = map unwords

format :: [String] -> [String]
format = prettyPrint . map (map (: []))