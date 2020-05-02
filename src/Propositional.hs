module Propositional ( Var
                     , Lit
                     , Clause(..)
                     , Formula
                     , FormulaState(..)
                     , fromLiterals
                     , isUnitState
                     , isUnsatState
                     , getUnitClause
                     , getChoiceResult
                     , currentFormula
                     , threadIn
                     , unitJustification
                     , arbitraryLiteral
                     , getLearnedClause
                     , clauseSize
                     , doesSatisfy
                     ) where

import qualified Data.Set as S
import Data.Maybe

type Var = Int

type Lit = Int

data Clause = Clause (S.Set Lit) [Lit] deriving (Show, Eq, Ord)

type Formula = [Clause]

data FormulaState = Initial Formula
                  | Derived Lit Formula (Maybe [Lit]) FormulaState
                  deriving (Show, Eq, Ord)

getChoiceResult :: Lit -> Maybe [Lit] -> FormulaState -> FormulaState
getChoiceResult l r fs = Derived l (setInFormula l (currentFormula fs)) r fs

fromLiterals :: [Lit] -> Clause
fromLiterals ls = Clause (S.fromList ls) ls

refreshClause :: Clause -> Clause
refreshClause (Clause _ ls) = fromLiterals ls

addLiteral :: Lit -> Clause -> Clause
addLiteral l cl@(Clause c cs)
    | (-l) `inClause` cl = fromLiterals []
    | otherwise = Clause (S.insert l c) (l:cs)

inClause :: Lit -> Clause -> Bool
inClause l (Clause c _) = l `S.member` c

removeLiteral :: Lit -> Clause -> Clause
removeLiteral l (Clause x y) = Clause (S.delete l x) y

isUnitClause :: Clause -> Bool
isUnitClause (Clause x _) = S.size x == 1

isUnitFormula :: Formula -> Bool
isUnitFormula = any isUnitClause

isUnitState :: FormulaState -> Bool
isUnitState = isUnitFormula . currentFormula

isUnsatClause :: Clause -> Bool
isUnsatClause (Clause x _) = S.null x

isUnsatFormula :: Formula -> Bool
isUnsatFormula = any isUnsatClause

isUnsatState :: FormulaState -> Bool
isUnsatState = isUnsatFormula . currentFormula

getUnitClause :: Formula -> (Clause, Formula)
getUnitClause = go isUnitClause where
    go f (x:xs)
        | f x = (x, xs)
        | otherwise = let (r, xs') = go f xs in (r, x:xs')


setInClause :: Lit -> Clause -> Maybe Clause
setInClause l c
    | l `inClause` c = Nothing
    | (-l) `inClause` c = Just $ removeLiteral (-l) c
    | otherwise = Just c

setInFormula :: Lit -> Formula -> Formula
setInFormula l f = mapMaybe (setInClause l) f

currentFormula :: FormulaState -> Formula
currentFormula fs = case fs of
    Initial f -> f
    Derived _ f _ _ -> f

getLearnedClause :: FormulaState -> Clause
getLearnedClause fs = go c fs where
    c = refreshClause $ head $ filter isUnsatClause $ currentFormula fs
    go c fs = case fs of
        Initial _ -> c
        Derived _ _ Nothing fs' -> go c fs'
        Derived l _ (Just r) fs'
            | (-l) `inClause` c -> go (foldr addLiteral (removeLiteral (-l) c) r) fs'
            | otherwise -> go c fs'

threadIn :: Clause -> FormulaState -> FormulaState
threadIn c fs = fst $ inner c fs where
    inner c (Initial f) = (Initial (c:f), Just c)
    inner c (Derived l f r fs) = (Derived l f' r fs', c'') where
        (fs', c') = inner c fs
        c'' = c' >>= setInClause l
        f' = case c'' of
            Nothing -> f
            Just x -> x:f

unitJustification :: Clause -> (Lit, [Lit])
unitJustification (Clause now init) = (lit, filter (/=lit) init) where
    lit = S.findMin now

arbitraryLiteral :: Clause -> Lit
arbitraryLiteral (Clause now init) = S.findMin now

clauseSize :: Clause -> Int
clauseSize (Clause c _) = length c

doesSatisfy :: [Lit] -> Formula -> Bool
doesSatisfy ls fs = isGood && all sat fs where
    s = S.fromList ls
    isGood = all (\l -> not $ (-l) `S.member` s) ls
    sat (Clause c _) = any (\l -> l `S.member` s) c
