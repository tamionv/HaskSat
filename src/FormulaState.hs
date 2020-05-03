module FormulaState ( FormulaState(..)
                    , buildDerivedState
                    , unitState
                    , unsatState
                    , satState
                    , applyUnitPropagation
                    , headForm
                    , getLearnedClause
                    , threadClauseInState
                    , arbitraryLiteral
                    , choices
                    ) where

import Propositional
import Data.List
import Data.Ord

data FormulaState = Initial Formula
                  | Derived Lit Formula (Maybe [Lit]) FormulaState
                  deriving (Show, Eq, Ord)

buildDerivedState :: Lit -> Maybe [Lit] -> FormulaState -> FormulaState
buildDerivedState l r fs = Derived l (setInFormula l (headForm fs)) r fs

unitState :: FormulaState -> Bool
unitState = any ((==1) . clauseSize) . headForm

unsatState :: FormulaState -> Bool
unsatState = any ((==0) . clauseSize) . headForm

satState :: FormulaState -> Bool
satState = null . headForm

applyUnitPropagation :: FormulaState -> FormulaState
applyUnitPropagation fs = fs' where
    c = head $ filter ((==1) . clauseSize) $ headForm fs
    lit = aClauseLit c
    r = filter (/=lit) $ initial c
    fs' = buildDerivedState lit (Just r) fs

headForm :: FormulaState -> Formula
headForm fs = case fs of
    Initial f -> f
    Derived _ f _ _ -> f

getLearnedClause :: FormulaState -> Clause
getLearnedClause fs = go c fs where
    c = refreshClause $ head $ filter ((==0) . clauseSize ) $ headForm fs
    go c fs = case fs of
        Initial _ -> c
        Derived _ _ Nothing fs' -> go c fs'
        Derived l _ (Just r) fs'
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'

threadClauseInState :: Clause -> FormulaState -> FormulaState
threadClauseInState c fs = fst $ inner c fs where
    inner c (Initial f) = (Initial (c:f), Just c)
    inner c (Derived l f r fs) = (Derived l f' r fs', c'') where
        (fs', c') = inner c fs
        c'' = c' >>= setInClause l
        f' = case c'' of
            Nothing -> f
            Just x -> x:f

arbitraryLiteral :: FormulaState -> Lit
arbitraryLiteral = aClauseLit . minimumBy (comparing $ length . current) . headForm where

choices :: FormulaState -> [Lit]
choices fs = case fs of
    Initial _ -> []
    Derived l _ _ fs' -> l:choices fs'
