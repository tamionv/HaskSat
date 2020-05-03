module Main where

import Control.Monad
import Control.Conditional (cond)
import Control.Exception
import Propositional
import CDCL

theAlgorithm :: CDCL [Lit]
theAlgorithm = do go ; liftReader getLiterals where
    go = do
        fs <- getState
        let cf = currentFormula fs
        cond [ (null cf         , return ())
             , (isUnsatState fs , failAndLearn)
             , (isUnitState fs  , unitProp)
             , (otherwise       , tryLiteral)
             ]

    choose lit r = modify $ getChoiceResult lit r

    unitProp = do
        cf <- currentFormula <$> getState
        let (c, _) = getUnitClause cf
        let (lit, r) = unitJustification c
        choose lit $ Just r

    failAndLearn = do
        c <- liftReader getLearnedClause
        failWithClause c

    tryLiteral = do
        lit <- liftReader arbitraryLiteral
        choiceFor lit `orElse` choiceFor (-lit) 

    choiceFor lit = do choose lit Nothing ; go

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
                return $ fromLiterals xs
            return (vars, nr, a)

main = do
    (vars, clauses, f) <- readInput
    let ret = runCDCL theAlgorithm (Initial f)
    case ret of
        Right (ls, _) -> do
            assert (doesSatisfy ls f) $ return ()
            putStrLn $ "s cnf 1 " ++ show vars ++ " " ++ show clauses
            forM_ ls $ \l -> putStrLn $ "v " ++ show l
        Left _ -> do
            putStrLn $ "s cnf 0 " ++ show vars ++ " " ++ show clauses
