module FormulaState ( FormulaState
                    , initialState
                    , derivedState
                    , unitState
                    , unsatState
                    , satState
                    , unitPropagation
                    , learnedClause
                    , addClause
                    , aFormulaStateLit
                    , choices
                    ) where

import Propositional
import Data.List
import Data.Ord

data FormulaState = Initial Formula
                  | Derived Lit Formula (Maybe [Lit]) FormulaState
                  deriving (Show, Eq, Ord)

headForm :: FormulaState -> Formula
headForm fs = case fs of
    Initial f -> f
    Derived _ f _ _ -> f

initialState :: Formula -> FormulaState
initialState f = Initial f

derivedState :: Lit -> Maybe [Lit] -> FormulaState -> FormulaState
derivedState l r fs = Derived l (setInFormula l (headForm fs)) r fs

unitState :: FormulaState -> Bool
unitState = any ((==1) . clauseSize) . headForm

unsatState :: FormulaState -> Bool
unsatState = any ((==0) . clauseSize) . headForm

satState :: FormulaState -> Bool
satState = null . headForm

unitPropagation :: FormulaState -> FormulaState
unitPropagation fs = derivedState lit (Just r) fs where
    c = head $ filter ((==1) . clauseSize) $ headForm fs
    lit = aClauseLit c
    r = filter (/=lit) $ initial c

learnedClause :: FormulaState -> Clause
learnedClause fs = go c fs where
    c = refreshClause $ head $ filter ((==0) . clauseSize ) $ headForm fs
    go c fs = case fs of
        Initial _ -> c
        Derived _ _ Nothing fs' -> go c fs'
        Derived l _ (Just r) fs'
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'

addClause :: Clause -> FormulaState -> FormulaState
addClause c fs = fst $ go c fs where
    go c (Initial f) = (Initial (c:f), Just c)
    go c (Derived l f r fs) = (Derived l f' r fs', c'') where
        (fs', c') = go c fs
        c'' = c' >>= setInClause l
        f' = case c'' of
            Nothing -> f
            Just x -> x:f

aFormulaStateLit :: FormulaState -> Lit
aFormulaStateLit = aClauseLit . minimumBy (comparing $ length . current) . headForm

choices :: FormulaState -> [Lit]
choices fs = case fs of
    Initial _ -> []
    Derived l _ _ fs' -> l:choices fs'
