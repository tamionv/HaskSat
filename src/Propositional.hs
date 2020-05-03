module Propositional ( Var
                     , Lit
                     , Clause(..)
                     , Formula
                     , buildClause
                     , refreshClause
                     , aClauseLit
                     , clauseSize
                     , addLiteral
                     , inClause
                     , removeLiteral
                     , setInClause
                     , setInFormula
                     , satisfies
                     ) where

import qualified Data.Set as S
import Data.Maybe

type Var = Int

type Lit = Int

data Clause = Clause { initial :: [Lit]
                     , current :: S.Set Lit
                     } deriving (Show, Eq, Ord)

type Formula = [Clause]

buildClause :: [Lit] -> Clause
buildClause ls = Clause { initial = ls
                         , current = S.fromList ls
                         }

aClauseLit :: Clause -> Lit
aClauseLit = S.findMin . current

clauseSize :: Clause -> Int
clauseSize = S.size . current

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

setInClause :: Lit -> Clause -> Maybe Clause
setInClause l c
    | l `inClause` c = Nothing
    | (-l) `inClause` c = Just $ removeLiteral (-l) c
    | otherwise = Just c

setInFormula :: Lit -> Formula -> Formula
setInFormula l f = mapMaybe (setInClause l) f

satisfies :: [Lit] -> Formula -> Bool
satisfies ls fs = isGood && all sat fs where
    s = S.fromList ls
    isGood = all (\l -> not $ (-l) `S.member` s) ls
    sat c = any (\l -> l `S.member` s) $ current c

