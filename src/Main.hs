module Main where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Loops (whileM_, untilM_)
import Control.Monad.Extra (ifM, notM)
import Debug.Trace
import Data.List
import Data.Ord
import Propositional

type CDCL a = State FormulaState a

choose :: Lit -> Maybe [Lit] -> CDCL ()
choose lit r = modify (getChoiceResult lit r)

backtrack :: CDCL ()
backtrack = modify (\d@(Derived _ _ _ fs) -> fs)

getFormula :: CDCL Formula
getFormula = liftM currentFormula get

isFinished :: CDCL Bool
isFinished = liftM null getFormula

isUnit :: CDCL Bool
isUnit = liftM isUnitState get

isUnsat :: CDCL Bool
isUnsat = liftM isUnsatState get

doUnitProp :: CDCL ()
doUnitProp = do 
    fs <- getFormula
    let (c, fs') = getUnitClause fs
    let (lit, r) = unitJustification c
    choose lit (Just r)


learnConflict :: CDCL ()
learnConflict = do
    fs <- get
    forM_ (getLearnedClauses fs) $ \c -> do
        modify (threadIn c)

getLiterals :: CDCL [Lit]
getLiterals = do
    fs <- get
    let go fs = case fs of
            Initial _ -> []
            Derived l _ _ fs' -> l:go fs'
    return $ go fs

getInitialFormula :: CDCL Formula
getInitialFormula = do
    fs <- get
    let go fs = case fs of
            Initial f -> f
            Derived _ _ _ fs' -> go fs
    return $ go fs

theAlgorithm :: CDCL [Lit]
theAlgorithm = do go ; getLiterals where
    go = ifM isFinished (return True) $ ifM isUnsat (do learnConflict ; return False) $ do
        fs <- getFormula
        let lit = arbitraryLiteral $ minimumBy (comparing clauseSize) fs
        choose lit Nothing
        ifM go (return True) $ do
            backtrack
            choose (-lit) Nothing
            ifM go (return True) $ do
                backtrack
                return False

main = do
    f <- readFormula
    let ret = evalState theAlgorithm (Initial f)
    print ret
