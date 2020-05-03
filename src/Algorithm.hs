module Algorithm (findSat) where

import Control.Monad
import Control.Conditional (cond)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Propositional
import FormulaState

algorithmAction :: StateT FormulaState (Either Clause) [Lit]
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
        throwError c

    tryLiteral = do
        lit <- gets aFormulaStateLit
        choose lit `catchError` \c -> do
            modify $ addClause c
            choose (-lit) 

    choose lit = do modify $ derivedState lit Nothing ; go

findSat :: Formula -> Maybe [Lit]
findSat f = case runStateT algorithmAction (initialState f) of
    Right (ls, _) -> Just ls
    Left _ -> Nothing
