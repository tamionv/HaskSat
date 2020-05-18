module Main where

import Control.Monad
import Control.Exception
import Debug.Trace
import Data.List.Split
import Data.Maybe
import Sat.Algorithm

type Sudoku = [Maybe Int]

charToCell :: Char -> Maybe Int
charToCell ch = case ch of
    '.' -> Nothing
    _ -> Just $ read $ [ch]

readSudoku :: IO Sudoku
readSudoku = do
    x <- getLine
    return $ map charToCell x

var :: Int -> Int -> Int -> Int
var j k i = -(81 * (i - 1) + 9 * (j - 1) + (k - 1) + 1)

pairs :: [a] -> [(a, a)]
pairs xs = go xs [] where
    go xs acc = case xs of
        [] -> acc
        x:xs -> go xs (map ((,) x) xs ++ acc)

neighbours :: [((Int, Int), (Int, Int))]
neighbours = do
    xSize <- [1, 3, 9]
    let ySize = 9 `div` xSize

    xs <- chunksOf xSize [1..9]
    ys <- chunksOf ySize [1..9]

    pairs [(x, y) | x <- xs, y <- ys]

baseFormula :: Formula
baseFormula = requirements ++ concatMap exclusionClause neighbours where
    exclusionClause ((i, j), (i', j')) =
        [ [ -var i j k, -var i' j' k ] | k <- [1..9] ]
    requirements =
        [ [ var i j k | k <- [1..9] ] | i <- [1..9], j <- [1..9] ]

getFormula :: Sudoku -> Formula
getFormula s = baseFormula ++ extra where
    extra = catMaybes $ zipWith clauseFor indices s
    indices = [ (i, j) | i <- [1..9], j <- [1..9] ]
    clauseFor (i, j) = fmap $ \k -> [var i j k]

interpretAssignment :: [Lit] -> [String]
interpretAssignment ls = do
    i <- [1..9]
    return $ do
        j <- [1..9]
        concatMap show $ filter (\k -> var i j k `elem` ls) $ [1..9]
        
main = do
    s <- readSudoku
    let Just assign = findSat $ getFormula s
    let s = interpretAssignment assign
    forM s putStrLn
