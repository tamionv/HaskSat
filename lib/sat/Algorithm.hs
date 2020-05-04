module Sat.Algorithm (findSat) where

import Control.Monad
import Control.Conditional (condM, otherwiseM)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Sat.Propositional
import Data.List
import Data.Ord
import Debug.Trace
import qualified Data.IntSet as S

data AlgState = Initial { headForm :: Formula }
                | Derived { headForm :: Formula
                          , decision :: Lit
                          , reason :: Maybe S.IntSet
                          , previousState :: AlgState
                          }
                deriving (Show, Eq, Ord)

initialState :: Formula -> AlgState
initialState x = Initial { headForm = x }

derivedState :: Lit -> Maybe S.IntSet -> AlgState -> AlgState
derivedState l r fs = Derived { headForm = setInFormula l (headForm fs)
                              , decision = l
                              , reason = r
                              , previousState = fs
                              }


unitPropagation :: AlgState -> AlgState
unitPropagation fs = foldr f fs
                   $ nubBy (\(x, _) (y, _) -> abs x == abs y)
                   $ map (\c -> (aClauseLit c, Just $ S.delete (aClauseLit c) $ initial c))
                   $ filter ((==1) . clauseSize)
                   $ headForm fs where
    f (l, r) fs = derivedState l r fs

learnedClauses :: AlgState -> [Clause]
learnedClauses fs = map (buildClause . flip go fs) cs where
    cs = map initial $ filter ((==0) . clauseSize) $ headForm fs
    go c fs = case fs of
        Derived { reason = Nothing } -> go c $ previousState fs
        Derived { decision = l, reason = Just r, previousState = fs' }
            | (-l) `S.member` c -> go (S.delete (-l) c `S.union` r) fs'
            | otherwise -> go c fs'
        _ -> c

addClause :: Clause -> AlgState -> AlgState
addClause c fs = fst $ go c fs where
    go c (Initial { headForm = f } ) = (initialState (c:f), Just c)
    go c fs@Derived{ headForm=f, decision=l, previousState=fs' }
        = (fs { previousState = fs', headForm = f'}, c'') where
            (fs'', c') = go c fs'
            c'' = c' >>= setInClause l
            f' = case c'' of
                Just x -> x:headForm fs'
                _ -> f

aLit :: AlgState -> Lit
aLit = aClauseLit . maximumBy (comparing clauseSize) . headForm

choices :: AlgState -> [Lit]
choices fs = case fs of
    Derived { decision=l, previousState=fs' } -> l:choices fs'
    _ -> []

algorithmAction :: StateT AlgState (Either [Clause]) [Lit]
algorithmAction = do go ; gets choices where
    go = do
        condM [ (isFinished       , return ())
              , (hasClauseOfSize 0, failAndLearn)
              , (hasClauseOfSize 1, unitProp)
              , (otherwiseM       , tryLiteral)
              ]

    isFinished = gets $ null . headForm

    hasClauseOfSize x = gets $ any ((==x) . clauseSize) . headForm 

    unitProp = do modify' unitPropagation ; go

    failAndLearn = gets learnedClauses >>= throwError

    tryLiteral = do
        lit <- gets aLit
        catchError (choose lit) $ mapM_ $ \c -> do
            when (not $ tautology c) $ modify' $ addClause c
            choose $ -lit

    choose lit = do modify' $ derivedState lit Nothing ; go

findSat :: Formula -> Maybe [Lit]
findSat f = case runStateT algorithmAction $ initialState f of
    Right (x, _) -> Just x
    _ -> Nothing
