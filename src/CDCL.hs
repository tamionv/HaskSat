module CDCL ( CDCL(..)
            , runCDCL
            , evalCDCL
            , gets
            , modify
            , orElse
            , failWithClause
            ) where

import Control.Monad
import Propositional

newtype CDCL a = CDCL (FormulaState -> Either Clause (a, FormulaState))

runCDCL :: CDCL a -> FormulaState -> Either Clause (a, FormulaState)
runCDCL (CDCL f) = f

evalCDCL :: CDCL a -> FormulaState -> a 
evalCDCL c fs = case runCDCL c fs of Right (a, _) -> a

gets :: (FormulaState -> a) -> CDCL a
gets f = CDCL $ \fs -> Right (f fs, fs)

modify :: (FormulaState -> FormulaState) -> CDCL ()
modify f = CDCL $ \fs -> Right ((), f fs)

orElse :: CDCL a -> CDCL a -> CDCL a
xm `orElse` ym = CDCL $ \fs -> case runCDCL xm fs of
    Right a -> Right a
    Left c -> runCDCL ym $ threadClauseInState c fs

failWithClause :: Clause -> CDCL a
failWithClause c = CDCL $ \_ -> Left c

instance Monad CDCL where
    return x = CDCL $ \fs -> Right (x, fs)

    xm >>= f = CDCL $ \fs -> case runCDCL xm fs of
        Right (x, fs') -> runCDCL (f x) fs'
        Left c -> Left c

instance Functor CDCL where
    fmap = liftM

instance Applicative CDCL where
    pure  = return
    (<*>) = ap
