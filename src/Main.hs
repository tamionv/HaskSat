module Main where

import Control.Monad
import Control.Conditional (cond)
import Debug.Trace
import Propositional

newtype CDCL a = CDCL (FormulaState -> Either (a, FormulaState) Clause)

runCDCL :: CDCL a -> FormulaState -> Either (a, FormulaState) Clause
runCDCL (CDCL f) = f

evalCDCL :: CDCL a -> FormulaState -> a 
evalCDCL c fs = case runCDCL c fs of Left (a, _) -> a

liftStateFunc :: (FormulaState -> a) -> CDCL a
liftStateFunc f = CDCL $ \fs -> Left (f fs, fs)

getState :: CDCL FormulaState
getState = liftStateFunc id

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

failAndLearn :: CDCL ()
failAndLearn = CDCL $ \fs -> Right $ getLearnedClause fs

theAlgorithm :: CDCL [Lit]
theAlgorithm = do go ; liftStateFunc getLiterals where
    go = do
        fs <- getState
        let cf = currentFormula fs
        cond [ (null cf         , return ())
             , (isUnsatState fs , failAndLearn)
             , (isUnitState fs  , unitProp)
             , (otherwise       , tryLiteral)
             ]

    unitProp = do
        cf <- currentFormula <$> getState
        let (c, _) = getUnitClause cf
        let (lit, r) = unitJustification c
        choose lit $ Just r

    tryLiteral = do
        lit <- liftStateFunc arbitraryLiteral
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
        Left (ls, _) -> do
            putStrLn $ "s cnf 1 " ++ show vars ++ " " ++ show clauses
            forM_ ls $ \l -> putStrLn $ "v " ++ show l
        Right _ -> do
            putStrLn $ "s cnf 0 " ++ show vars ++ " " ++ show clauses
