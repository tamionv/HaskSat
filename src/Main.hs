module Main where

import Control.Monad
import Control.Conditional (condM, otherwiseM)
import Debug.Trace
import Data.List
import Data.Ord
import Propositional

newtype CDCL a = CDCL (FormulaState -> Either (a, FormulaState) Clause)

runCDCL :: CDCL a -> FormulaState -> Either (a, FormulaState) Clause
runCDCL (CDCL f) = f

evalCDCL :: CDCL a -> FormulaState -> a 
evalCDCL c fs = case runCDCL c fs of Left (a, _) -> a

liftStateFunc :: (FormulaState -> a) -> CDCL a
liftStateFunc f = CDCL $ \fs -> Left (f fs, fs)

instance Monad CDCL where
    return x = CDCL $ \fs -> Left (x, fs)

    xm >>= f = CDCL $ \fs -> case runCDCL xm fs of
        Left (x, fs') -> runCDCL (f x) fs'
        Right c -> Right c

instance Functor CDCL where
    fmap = liftM

instance Applicative CDCL where
    pure  = return
    (<*>) = ap

choose :: Lit -> Maybe [Lit] -> CDCL ()
choose lit r = CDCL $ \fs -> Left ((), getChoiceResult lit r fs)

orElse :: CDCL a -> CDCL a -> CDCL a
xm `orElse` ym = CDCL $ \fs -> case runCDCL xm fs of
    Left a -> Left a
    Right c -> runCDCL ym $ threadIn c fs

isFinished :: CDCL Bool
isFinished = liftStateFunc (null . currentFormula)

isUnit :: CDCL Bool
isUnit = liftStateFunc isUnitState

isUnsat :: CDCL Bool
isUnsat = liftStateFunc isUnsatState

doUnitProp :: CDCL ()
doUnitProp = liftStateFunc currentFormula >>= go where
    go fs = choose lit $ Just r where
        (c, fs') = getUnitClause fs
        (lit, r) = unitJustification c

failAndLearn :: CDCL ()
failAndLearn = CDCL $ \fs -> Right $ getLearnedClause fs

getLiterals :: CDCL [Lit]
getLiterals = liftStateFunc go where
    go fs = case fs of
        Initial _ -> []
        Derived l _ _ fs' -> l:go fs'

getInitialFormula :: CDCL Formula
getInitialFormula = liftStateFunc go where
    go fs = case fs of
        Initial f -> f
        Derived _ _ _ fs' -> go fs

theAlgorithm :: CDCL [Lit]
theAlgorithm = do go ; getLiterals where
    go = condM [ (isFinished, return ())
               , (isUnsat   , failAndLearn)
               , (isUnit    , do doUnitProp ; go)
               , (otherwiseM, doMainAttempt)
               ]
    choiceFor lit = do choose lit Nothing ; go
    doMainAttempt = do
        x <- getLiterals
        trace (show $ length x) $ return ()
        lit <- liftStateFunc $ arbitraryLiteral . minimumBy (comparing clauseSize) . currentFormula
        choiceFor lit `orElse` choiceFor (-lit) 

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
        Left (ls, _) -> do
            putStrLn $ "s cnf 1 " ++ show vars ++ " " ++ show clauses
            forM_ ls $ \l -> putStrLn $ "v " ++ show l
        Right _ -> do
            putStrLn $ "s cnf 0 " ++ show vars ++ " " ++ show clauses
