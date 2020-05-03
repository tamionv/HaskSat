module Algorithm (findSat) where

import Control.Monad
import Control.Conditional (cond)
import Control.Monad.State.Lazy
import Control.Monad.Except
import Propositional
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
        Initial _ -> c
        Derived _ _ Nothing fs' -> go c fs'
        Derived l _ (Just r) fs'
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'

addClause :: Clause -> AlgState -> AlgState
addClause c fs = fst $ go c fs where
    go c (Initial f) = (Initial (c:f), Just c)
    go c (Derived l f r fs) = (Derived l f' r fs', c'') where
        (fs', c') = go c fs
        c'' = c' >>= setInClause l
        f' = case c'' of
            Nothing -> f
            Just x -> x:f

aLit :: AlgState -> Lit
aLit = aClauseLit . minimumBy (comparing $ length . current) . headForm

choices :: AlgState -> [Lit]
choices fs = case fs of
    Initial _ -> []
    Derived l _ _ fs' -> l:choices fs'

algorithmAction :: StateT AlgState (Either Clause) [Lit]
algorithmAction = do go ; gets choices where
    go = do
        f <- gets headForm
        cond [ (null f                    , return ())
             , (any ((==0) . clauseSize) f, failAndLearn)
             , (any ((==1) . clauseSize) f, unitProp)
             , (otherwise                 , tryLiteral)
             ]

    unitProp = modify unitPropagation

    failAndLearn = do
        c <- gets learnedClause
        throwError c

    tryLiteral = do
        lit <- gets aLit
        choose lit `catchError` \c -> do
            modify $ addClause c
            choose (-lit) 

    choose lit = do modify $ derivedState lit Nothing ; go

findSat :: Formula -> Maybe [Lit]
findSat f = case runStateT algorithmAction (Initial f) of
    Right (ls, _) -> Just ls
    Left _ -> Nothing
