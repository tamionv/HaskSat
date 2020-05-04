module Main where

import Control.Monad
import Control.Exception
import Data.Maybe
import Propositional
import Algorithm

type Sudoku = [[Maybe Int]]

charToCell :: Char -> Maybe Int
charToCell ch = case ch of
    '.' -> Nothing
    _ -> Just $ read $ ch:[]

readSudoku :: IO Sudoku
readSudoku = forM [1..9] $ \_ -> do
    x <- getLine
    return $ map charToCell x

var i j k = 81 * (i - 1) + 9 * (j - 1) + (k - 1) + 1

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs

neighbours :: [((Int, Int), (Int, Int))]
neighbours = vert ++ hor ++ box where
    vertSet = [ [(i, j) | j <- [1..9]] | i <- [1..9]]
    horSet = [ [(i, j) | i <- [1..9]] | j <- [1..9]]
    boxSet = do
        xs <- [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        ys <- [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        return $ do
            x <- xs
            y <- ys
            return (x, y)

    vert = concatMap allPairs vertSet
    hor = concatMap allPairs horSet
    box = concatMap allPairs boxSet

baseFormula :: Formula
baseFormula = requirements ++ concatMap f neighbours where
    f ((i, j), (i', j')) = [ buildClause [ -var i j x, - var i' j' x ]
                           | x <- [1..9]
                           ]
    requirements = [ buildClause [ var i j k | k <- [1..9] ] | i <- [1..9], j <- [1..9] ]

getFormula :: Sudoku -> Formula
getFormula s = baseFormula ++ extra where
    indexed = zipWith zip [ [(i, j) | j <- [1..9]] | i <- [1..9]] s
    together = concat indexed

    indexedToFormula x = case x of
        ((i, j), Just y) -> Just $ buildClause [var i j y]
        _ -> Nothing

    extra = mapMaybe indexedToFormula together

interpretSudoku :: [Lit] -> [String]
interpretSudoku ls = do
    i <- [1..9]
    return $ do
        j <- [1..9]
        concatMap show $ filter (\k -> var i j k `elem` ls) $ [1..9]
        
main = do
    s <- readSudoku
    let Just assign = findSat $ getFormula s
    let s = interpretSudoku assign
    forM s putStrLn
