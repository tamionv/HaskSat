module Sat.Algorithm ( findSat
                     , Var
                     , Lit
                     , Clause
                     , Formula ) where

import Control.Monad
import Control.Applicative
import Control.Conditional (ifM)
import Control.Monad.State
import Data.List
import Data.Ord
import Data.Maybe
import Debug.Trace
import qualified Data.IntMap as M

type Var = Int

type Lit = Int

type Clause = [Lit]

type Formula = [Clause]

data Maplet = Maplet { value :: !Bool
                     , reason :: !(Maybe Clause)
                     }

type Assignment = M.IntMap Maplet

type AlgState = (Formula, Assignment)

newtype AlgMonad x = AlgMonad { runMonad :: forall r. AlgState -> (AlgState -> x -> r) -> (Formula -> r) -> r }

instance Monad AlgMonad where
    return x = AlgMonad $ \(!s) k kf -> k s x
    xm >>= f = AlgMonad $ \(!s@(_, a)) k kf -> runMonad xm s (\(!s') (!x) -> runMonad (f x) s' k kf) kf

instance MonadPlus AlgMonad where
    mzero = AlgMonad $ \((!f), _) _ kf -> kf f

    xm `mplus` ym = AlgMonad $ \(!s@(_, a)) k kf ->
        runMonad xm s k (\(!f') -> runMonad ym (f', a) k kf)

instance MonadState AlgState AlgMonad where
    get = AlgMonad $ \(!s) k kf -> k s s
    put s = AlgMonad $ \_ k kf -> k s ()

instance Functor AlgMonad where
    fmap = liftM

instance Applicative AlgMonad where
    (<*>) = ap
    pure = return

instance Alternative AlgMonad where
    empty = mzero
    (<|>) = mplus

choices :: Assignment -> [Lit]
choices !a = map interpret $ M.assocs a where
    interpret (v, mp) | value mp = v
                      | otherwise = -v

varOfLit :: Lit -> Var
varOfLit !l = abs l

polarityOfLit :: Lit -> Bool
polarityOfLit !l = l > 0

addMaplet :: Lit -> Maybe Clause -> Assignment -> Assignment
addMaplet !l !r !a = M.insert v m a where
    v = varOfLit l
    m = Maplet { value = polarityOfLit l
               , reason = r
               }

isMentioned :: Lit -> Assignment -> Bool
isMentioned !l !a = isJust $ M.lookup (varOfLit l) a

lookupLit :: Lit -> Assignment -> Maybe Bool
lookupLit !l !a = fmap ((==polarityOfLit l) . value) $ M.lookup (varOfLit l) a

clauseAfterAssignment :: Assignment -> Clause -> Maybe Clause
clauseAfterAssignment !a !c
    | isSat = Nothing
    | otherwise = Just simplified where 
        isSat = any id $ catMaybes $ map (`lookupLit` a) c
        simplified = filter (not . (`isMentioned` a)) c

formulaAfterAssignment :: Formula -> Assignment -> Formula
formulaAfterAssignment !f !a = catMaybes
                             $ map (clauseAfterAssignment a) f

unitPropagation :: AlgState -> AlgState
unitPropagation (!f, !a) = (f, a') where
    lits = nubBy (\(x, _) (y, _) -> varOfLit x == varOfLit y)
         $ map (\([x], y) -> (x, Just $ filter (/=x) y))
         $ filter ((==1) . length . fst)
         $ catMaybes
         $ map (\c -> do x <- clauseAfterAssignment a c; return (x, c)) f
    a' = foldr (uncurry addMaplet) a lits

addClause :: Clause -> AlgState -> AlgState
addClause !c (!f, !a) = (c:f, a)

chooseLit :: AlgState -> Lit
chooseLit (!f, !a) = head
                   $ minimumBy (comparing length)
                   $ formulaAfterAssignment f a

learnedClauses :: AlgState -> [Clause]
learnedClauses (!f, !a) = map getConflict
                        $ take 1
                        $ filter ((==Just[]) . clauseAfterAssignment a) f where
    getConflict [] = []
    getConflict (c:cs) = case M.lookup (varOfLit c) a >>= reason of
        Just r -> getConflict $ r ++ cs
        Nothing -> c : getConflict cs

satisfied :: AlgState -> Bool
satisfied (!f, !a) = formulaAfterAssignment f a == []

unsatisfied :: AlgState -> Bool
unsatisfied (!f, !a) = any (==[]) $ formulaAfterAssignment f a

hasUnit :: AlgState -> Bool
hasUnit (!f, !a) = any ((==1) . length) $ formulaAfterAssignment f a

algorithmAction :: AlgMonad [Lit]
algorithmAction = go where
    go = get >>= mainCond

    mainCond s
        | satisfied s = gets (choices . snd)
        | unsatisfied s = learnAndFail
        | hasUnit s = unitProp
        | otherwise = tryLiteral

    unitProp = do modify' unitPropagation ; go

    learnAndFail = do
        cs <- gets learnedClauses
        forM_ cs $ \c -> when (not $ null c) $ modify $ addClause c
        mzero

    tryLiteral = gets chooseLit >>= exhaust

    choose lit = do modify' $ \(f, a) -> (f, addMaplet lit Nothing a) ; go

    exhaust lit = choose lit `mplus` ifM (gets $ not . unsatisfied) (choose (-lit)) mzero

findSat :: Formula -> Maybe [Lit]
findSat f = runMonad algorithmAction (f, M.empty) (\_ x -> Just x) (\_ -> Nothing)
