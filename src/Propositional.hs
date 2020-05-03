module Propositional ( Var
                     , Lit
                     , Clause(..)
                     , Formula
                     , FormulaState(..)
                     , buildClause
                     , unitState
                     , unsatState
                     , satState
                     , buildDerivedState
                     , threadClauseInState
                     , applyUnitPropagation
                     , arbitraryLiteral
                     , getLearnedClause
                     , satisfies
                     , choices
                     ) where

import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Ord

type Var = Int

type Lit = Int

data Clause = Clause { initial :: [Lit]
                     , current :: S.Set Lit
                     } deriving (Show, Eq, Ord)

type Formula = [Clause]

data FormulaState = Initial Formula
                  | Derived Lit Formula (Maybe [Lit]) FormulaState
                  deriving (Show, Eq, Ord)

buildDerivedState :: Lit -> Maybe [Lit] -> FormulaState -> FormulaState
buildDerivedState l r fs = Derived l (setInFormula l (headForm fs)) r fs

buildClause :: [Lit] -> Clause
buildClause ls = Clause { initial = ls
                         , current = S.fromList ls
                         }

refreshClause :: Clause -> Clause
refreshClause = buildClause . initial

addLiteral :: Lit -> Clause -> Clause
addLiteral l c
    | (-l) `inClause` c = buildClause []
    | otherwise = Clause { initial = l:initial c
                         , current = S.insert l $ current c
                         }

inClause :: Lit -> Clause -> Bool
inClause l c = l `S.member` current c

removeLiteral :: Lit -> Clause -> Clause
removeLiteral l c = c { current = S.delete l $ current c }

unitState :: FormulaState -> Bool
unitState = any ((==1) . S.size . current) . headForm

unsatState :: FormulaState -> Bool
unsatState = any (S.null . current) . headForm

satState :: FormulaState -> Bool
satState = null . headForm

applyUnitPropagation :: FormulaState -> FormulaState
applyUnitPropagation fs = fs' where
    c = head $ filter ((==1) . S.size . current) $ headForm fs
    lit = S.findMin $ current c
    r = filter (/=lit) $ initial c
    fs' = buildDerivedState lit (Just r) fs

setInClause :: Lit -> Clause -> Maybe Clause
setInClause l c
    | l `inClause` c = Nothing
    | (-l) `inClause` c = Just $ removeLiteral (-l) c
    | otherwise = Just c

setInFormula :: Lit -> Formula -> Formula
setInFormula l f = mapMaybe (setInClause l) f

headForm :: FormulaState -> Formula
headForm fs = case fs of
    Initial f -> f
    Derived _ f _ _ -> f

getLearnedClause :: FormulaState -> Clause
getLearnedClause fs = go c fs where
    c = refreshClause $ head $ filter (S.null . current) $ headForm fs
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
arbitraryLiteral = S.findMin . current . minimumBy (comparing $ length . current) . headForm where

satisfies :: [Lit] -> Formula -> Bool
satisfies ls fs = isGood && all sat fs where
    s = S.fromList ls
    isGood = all (\l -> not $ (-l) `S.member` s) ls
    sat c = any (\l -> l `S.member` s) $ current c

choices :: FormulaState -> [Lit]
choices fs = case fs of
    Initial _ -> []
    Derived l _ _ fs' -> l:choices fs'
