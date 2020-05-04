module Sat.Algorithm (findSat) where

import Control.Monad
import Control.Conditional (condM, otherwiseM)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Sat.Propositional
import Data.List
import Data.Ord

data AlgState = Initial Formula
                  | Derived Lit Formula (Maybe [Lit]) AlgState
                  deriving (Show, Eq, Ord)

derivedState :: Lit -> Maybe [Lit] -> AlgState -> AlgState
derivedState l r fs = Derived l (setInFormula l (headForm fs)) r fs

headForm :: AlgState -> Formula
headForm fs = case fs of
    Initial f -> f
    Derived _ f _ _ -> f

unitPropagation :: AlgState -> AlgState
unitPropagation fs = derivedState lit (Just r) fs where
    c = head $ filter ((==1) . clauseSize) $ headForm fs
    lit = aClauseLit c
    r = filter (/=lit) $ initial c

learnedClause :: AlgState -> Clause
learnedClause fs = go c fs where
    c = refreshClause $ head $ filter ((==0) . clauseSize ) $ headForm fs
    go c fs = case fs of
        Derived _ _ Nothing fs' -> go c fs'
        Derived l _ (Just r) fs'
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'
        _ -> c

addClause :: Clause -> AlgState -> AlgState
addClause c fs = fst $ go c fs where
    go c (Initial f) = (Initial (c:f), Just c)
    go c (Derived l f r fs) = (Derived l f' r fs', c'') where
        (fs', c') = go c fs
        c'' = c' >>= setInClause l
        f' = case c'' of
            Just x -> x:f
            _ -> f

aLit :: AlgState -> Lit
aLit = aClauseLit . minimumBy (comparing $ length . current) . headForm

choices :: AlgState -> [Lit]
choices fs = case fs of
    Derived l _ _ fs' -> l:choices fs'
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
findSat f = case runStateT algorithmAction $ Initial f of
    Right (x, _) -> Just x
    _ -> Nothing
