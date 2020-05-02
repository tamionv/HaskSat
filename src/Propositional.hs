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
                     , doesSatisfy
                     , getLiterals
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

getChoiceResult :: Lit -> Maybe [Lit] -> FormulaState -> FormulaState
getChoiceResult l r fs = Derived l (setInFormula l (currentFormula fs)) r fs

fromLiterals :: [Lit] -> Clause
fromLiterals ls = Clause { initial = ls
                         , current = S.fromList ls
                         }

refreshClause :: Clause -> Clause
refreshClause = fromLiterals . initial

addLiteral :: Lit -> Clause -> Clause
addLiteral l c
    | (-l) `inClause` c = fromLiterals []
    | otherwise = Clause { initial = l:initial c
                         , current = S.insert l $ current c
                         }

inClause :: Lit -> Clause -> Bool
inClause l c = l `S.member` current c

removeLiteral :: Lit -> Clause -> Clause
removeLiteral l c = c { current = S.delete l $ current c }

isUnitClause :: Clause -> Bool
isUnitClause c = S.size (current c) == 1

isUnitState :: FormulaState -> Bool
isUnitState = any isUnsatClause . currentFormula

isUnsatClause :: Clause -> Bool
isUnsatClause c = S.null $ current c

isUnsatState :: FormulaState -> Bool
isUnsatState = any isUnsatClause . currentFormula

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
unitJustification c = (lit, filter (/=lit) $ initial c) where
    lit = S.findMin $ current c

arbitraryLiteral :: FormulaState -> Lit
arbitraryLiteral = S.findMin . current . minimumBy (comparing $ length . current) . currentFormula where

doesSatisfy :: [Lit] -> Formula -> Bool
doesSatisfy ls fs = isGood && all sat fs where
    s = S.fromList ls
    isGood = all (\l -> not $ (-l) `S.member` s) ls
    sat c = any (\l -> l `S.member` s) $ current c

getLiterals :: FormulaState -> [Lit]
getLiterals fs = case fs of
    Initial _ -> []
    Derived l _ _ fs' -> l:getLiterals fs'
