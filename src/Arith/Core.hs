module Arith.Core where

import           Control.Monad                  ( forM_
                                                , when
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import           Arith.Abs                      ( Exp(..) )
import           Arith.Lex                      ( Token
                                                , mkPosToken
                                                )
import           Arith.Par                      ( myLexer )
import           Arith.Print                    ( Print
                                                , printTree
                                                )
import           Arith.Skel                     ( )
import           System.Console.Haskeline       ( )
import           Types                          ( EvaluationError
                                                  ( EvaluationError
                                                  )
                                                , GExp(eval1)
                                                , GToken(..)
                                                )


isNumericVal :: Exp -> Bool
isNumericVal EZero     = True
isNumericVal (ESucc t) = isNumericVal t
isNumericVal _         = False

isVal :: Exp -> Bool
isVal ETrue  = True
isVal EFalse = True
isVal t      = isNumericVal t

instance GExp Exp where
  eval1 (ECond ETrue  t2 _ ) = Right t2
  eval1 (ECond EFalse _  t3) = Right t3
  eval1 (ECond t1 t2 t3) =
    let t1' = eval1 t1 in t1' >>= Right . (\x -> ECond x t2 t3)
  eval1 (ESucc t1   ) = let t1' = eval1 t1 in t1' >>= Right . ESucc
  eval1 (EPred EZero) = Right EZero
  eval1 (EPred (ESucc t)) =
    if isNumericVal t then Right t else Left (EvaluationError $ show t)
  eval1 (EIsZero EZero ) = Right ETrue
  eval1 (EIsZero ETrue ) = Right EFalse
  eval1 (EIsZero EFalse) = Right EFalse
  eval1 (EIsZero (ESucc t)) =
    if isNumericVal t then Right EFalse else Left (EvaluationError $ show t)
  eval1 (EIsZero t1) = let t1' = eval1 t1 in t1' >>= Right . EIsZero
  eval1 _            = Left $ EvaluationError "Normal form term"

instance GToken Token where
  gLexer      = myLexer
  gMkPosToken = mkPosToken
