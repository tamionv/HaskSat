module Main where

import Control.Monad
import Control.Conditional (cond)
import Control.Exception
import Propositional
import FormulaState
import CDCL

theAlgorithm :: CDCL [Lit]
theAlgorithm = do go ; gets choices where
    go = do
        fs <- gets id
        cond [ (satState fs  , return ())
             , (unsatState fs, failAndLearn)
             , (unitState fs , unitProp)
             , (otherwise    , tryLiteral)
             ]

    unitProp = modify unitPropagation

    failAndLearn = do
        c <- gets learnedClause
        failWithClause c

    tryLiteral = do
        lit <- gets aFormulaStateLit
        choose lit `orElse` choose (-lit) 

    choose lit = do modify $ derivedState lit Nothing ; go

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
                return $ buildClause xs
            return (vars, nr, a)

main = do
    (vars, clauses, f) <- readInput
    let ret = runCDCL theAlgorithm (initialState f)
    case ret of
        Right (ls, _) -> do
            assert (satisfies ls f) $ return ()
            putStrLn $ "s cnf 1 " ++ show vars ++ " " ++ show clauses
            forM_ ls $ \l -> putStrLn $ "v " ++ show l
        Left _ -> do
            putStrLn $ "s cnf 0 " ++ show vars ++ " " ++ show clauses
