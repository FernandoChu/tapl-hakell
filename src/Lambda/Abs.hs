-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

-- | The abstract syntax of language lambda.

module Lambda.Abs where

import qualified Prelude as C (Eq, Ord, Show, Read)

data Exp
    = ETrue
    | EFalse
    | ECond Exp Exp Exp
    | EZero
    | ESucc Exp
    | EPred Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)
