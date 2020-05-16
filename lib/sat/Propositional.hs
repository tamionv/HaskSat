module Sat.Propositional
    ( Var
    , Lit
    , Clause(..)
    , Formula
    , buildClause
    , aClauseLit
    , clauseSize
    , addLiteral
    , inClause
    , removeLiteral
    , setInClause
    , setInFormula
    , tautology
    , satisfies
    , fromList
    ) where

import qualified Data.IntSet as S
import Data.Maybe

type Var = Int

type Lit = Int

data Clause = Clause { initial :: !S.IntSet
                     , current :: !S.IntSet 
                     } deriving (Show, Eq, Ord)

type Formula = [Clause]

fromList :: [Lit] -> Clause
fromList !xs = buildClause $! S.fromList xs

buildClause :: S.IntSet -> Clause
buildClause !ls = Clause { initial = ls
                         , current = ls
                         }

aClauseLit :: Clause -> Lit
aClauseLit !c = S.findMin $! current c

clauseSize :: Clause -> Int
clauseSize !c = S.size $! current c

addLiteral :: Lit -> Clause -> Clause
addLiteral !l !c = Clause { initial = S.insert l $! initial c
                          , current = S.insert l $! current c
                          }

tautology :: Clause -> Bool
tautology !c = any (`inClause` c) $! map negate $! S.toList $! current c

inClause :: Lit -> Clause -> Bool
inClause !l !c = l `S.member` current c

removeLiteral :: Lit -> Clause -> Clause
removeLiteral !l !c = c { current = S.delete l $! current c }

setInClause :: Lit -> Clause -> Maybe Clause
setInClause !l !c
    | l `inClause` c = Nothing
    | (-l) `inClause` c = Just $! removeLiteral (-l) c
    | otherwise = Just c

setInFormula :: Lit -> Formula -> Formula
setInFormula !l !f = mapMaybe (setInClause l) f

satisfies :: [Lit] -> Formula -> Bool
satisfies !ls !fs = isGood && all sat fs where
    s = S.fromList ls
    isGood = all (\l -> not $! (-l) `S.member` s) ls
    sat c = any (\l -> l `S.member` s) $! S.toList $! current c
