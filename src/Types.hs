module TaplTypes where

data Verbosity = Normal | Verbose

type Err = Either String
-- type ParseFun tok a = [tok] -> Err a

newtype EvaluationError = EvaluationError String
  deriving (Show)

-- class GExp a where
--     eval1 :: a -> Either EvaluationError a
--     eval :: Verbosity -> a -> [a]
--     eval v t = accEval v t [t]
--       where
--         accEval v t l = case eval1 t of
--             Left  err -> l
--             Right val -> accEval v val (val : l)
