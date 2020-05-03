module Algorithm (findSat) where

import Control.Monad
import Control.Conditional (cond)
import Propositional
import FormulaState
import CDCL

algorithmAction :: CDCL [Lit]
algorithmAction = do go ; gets choices where
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

findSat :: Formula -> Maybe [Lit]
findSat f = case runCDCL algorithmAction (initialState f) of
    Right (ls, _) -> Just ls
    Left _ -> Nothing
