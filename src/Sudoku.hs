module Sudoku where

import Data.List ((\\))
import Data.List.Split (chunksOf)

type Matrix a = [[a]]

type Board = Matrix Char

type Row a = [a]

type Choices = [Char]

boardSize :: Integer
boardSize = 9

boxSize :: Int
boxSize = 3

cellVals :: Choices
cellVals = "123456789"

sudoku = search . prune . choices

isBlank :: Char -> Bool
isBlank = (== '.')

-- check if no duplicates
noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x : xs) = x `notElem` xs && noDups xs

correct :: Eq a => [[a]] -> Bool
correct board = all noDups (rows board) && all noDups (cols board) && all noDups (boxs board)

-- extraction functions
rows :: a -> a
rows = id

cols :: [[a]] -> [[a]]
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: [[a]] -> [[a]]
boxs = map ungroup . ungroup . map cols . group' . map group'

group' :: [e] -> [[e]]
group' = chunksOf boxSize

ungroup :: [[a]] -> [a]
ungroup = concat

-- generate search space
choices :: [String] -> [[Choices]]
choices = map (map choose)

choose :: Char -> Choices
choose e = if isBlank e then cellVals else [e]

-- matrix cartesian product
mcp :: [[[a]]] -> [[[a]]]
mcp = cp . map cp

-- cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]

-- pruning functions to reduce the search space
fixed :: [Choices] -> Choices
fixed = concat . filter single

single :: [a] -> Bool
single = (== 1) . length

reduce :: [Choices] -> [Choices]
reduce choicesList = [xs `minus` singles | xs <- choicesList]
  where
    singles = concat (filter single choicesList)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy rows . pruneBy cols . pruneBy boxs

pruneBy :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f = f . map reduce . f

-- reports if a Matrix is unsolvable
blocked :: Matrix Choices -> Bool
blocked choiceMatrix = nullish choiceMatrix || not (safe choiceMatrix)

nullish :: Matrix Choices -> Bool
nullish = any (any null)

-- for all rows/cols/boxes, all the fixed cells contain no duplicates
safe :: Matrix Choices -> Bool
safe choiceMatrix = all (noDups . fixed) (rows choiceMatrix) && all (noDups . fixed) (cols choiceMatrix) && all (noDups . fixed) (boxs choiceMatrix)

expand :: Matrix Choices -> Matrix [Choices]
expand choiceMatrix = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (not . all single) choiceMatrix
    (row1, cs : row2) = break (not . single) row

minChoice :: [[Choices]] -> Int
minChoice = minimum . filter (> 1) . concatMap (map length)

search :: Matrix Choices -> [Matrix Choices]
search choiceMatrix
  | blocked choiceMatrix = []
  | all (all single) choiceMatrix = [choiceMatrix]
  | otherwise = (concatMap (search . prune) . expand) choiceMatrix