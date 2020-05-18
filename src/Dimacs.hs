module Main where

import Control.Monad
import Control.Exception
import Sat.Algorithm

readInput :: IO (Int, Int, Formula)
readInput = do
    x <- getLine
    let xs = words x
    case head xs of
        "c" -> readInput
        "p" -> do
            let _:_:vars:nr:_ = map read xs :: [Int]
            a <- sequence $ take nr $ repeat $ do
                x <- getLine
                let xs = (map read $ init $ words x) :: [Int]
                return xs
            return (vars, nr, a)

main = do
    (vars, clauses, f) <- readInput
    let ret = findSat f
    case ret of
        Just ls -> do
            putStrLn $ "s cnf 1 " ++ show vars ++ " " ++ show clauses
            forM_ ls $ \l -> putStrLn $ "v " ++ show l
        Nothing -> do
            putStrLn $ "s cnf 0 " ++ show vars ++ " " ++ show clauses
