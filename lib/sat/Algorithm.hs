module Sat.Algorithm (findSat) where

import Control.Monad
import Control.Conditional (condM, otherwiseM)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Sat.Propositional
import Data.List
import Data.Ord

data AlgState = Initial { headForm :: Formula }
                | Derived { headForm :: Formula
                          , decision :: Lit
                          , reason :: Maybe [Lit]
                          , previousState :: AlgState
                          }
                deriving (Show, Eq, Ord)

initialState :: Formula -> AlgState
initialState x = Initial { headForm = x }

derivedState :: Lit -> Maybe [Lit] -> AlgState -> AlgState
derivedState l r fs = Derived { headForm = setInFormula l (headForm fs)
                              , decision = l
                              , reason = r
                              , previousState = fs
                              }


unitPropagation :: AlgState -> AlgState
unitPropagation fs = derivedState lit (Just r) fs where
    c = head $ filter ((==1) . clauseSize) $ headForm fs
    lit = aClauseLit c
    r = filter (/=lit) $ initial c

learnedClause :: AlgState -> Clause
learnedClause fs = go c fs where
    c = refreshClause $ head $ filter ((==0) . clauseSize ) $ headForm fs
    go c fs = case fs of
        Derived { reason = Nothing } -> go c $ previousState fs
        Derived { decision = l, reason = Just r, previousState = fs' }
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'
        _ -> c

addClause :: Clause -> AlgState -> AlgState
addClause c fs = fst $ go c fs where
    go c (Initial { headForm = f } ) = (initialState (c:f), Just c)
    go c fs@Derived{ headForm=f, decision=l, previousState=fs' }
        = (derivedState l (reason fs) fs'', c'') where
            (fs'', c') = go c fs'
            c'' = c' >>= setInClause l
            f' = case c'' of
                Just x -> x:headForm fs'
                _ -> f

aLit :: AlgState -> Lit
aLit = aClauseLit . minimumBy (comparing $ length . current) . headForm

choices :: AlgState -> [Lit]
choices fs = case fs of
    Derived { decision=l, previousState=fs' } -> l:choices fs'
    _ -> []

algorithmAction :: StateT AlgState (Either Clause) [Lit]
algorithmAction = do go ; gets choices where
    go = condM [ (isFinished       , return ())
               , (hasClauseOfSize 0, failAndLearn)
               , (hasClauseOfSize 1, unitProp)
               , (otherwiseM       , tryLiteral)
               ]

    isFinished = gets $ null . headForm

    hasClauseOfSize x = gets $ any ((==x) . clauseSize) . headForm 

    unitProp = do modify unitPropagation ; go

    failAndLearn = gets learnedClause >>= throwError

    tryLiteral = do
        lit <- gets aLit
        choose lit `catchError` \c -> do
            when (not $ tautology c) $ modify $ addClause c
            choose $ -lit

    choose lit = do modify $ derivedState lit Nothing ; go

findSat :: Formula -> Maybe [Lit]
findSat f = case runStateT algorithmAction $ initialState f of
    Right (x, _) -> Just x
    _ -> Nothing
