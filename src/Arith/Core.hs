-- Program to test parser, automatically generated by BNF Converter.

module Arith.Core where

import           Control.Monad                  ( forM_
                                                , when
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import           Arith.Abs
import           Arith.Lex                      ( Token
                                                , mkPosToken
                                                )
import           Arith.Par
import           Arith.Print                    ( Print
                                                , printTree
                                                )
import           Arith.Skel                     ( )
import           System.Console.Haskeline
import           TaplTypes

type ParseFun a = [Token] -> Err a

putStrV :: Verbosity -> String -> IO ()
putStrV Normal  s = pure ()
putStrV Verbose s = putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = case p ts of
  Left err -> do
    putStrLn "\nParse              Failed...\n"
    putStrV v "Tokens:"
    mapM_ (putStrV v . showPosToken . mkPosToken) ts
    putStrV v "Tokens:"
    putStrLn err
  Right tree -> do
    showTree v tree
    forM_ (reverse (eval v tree)) print
 where
  ts = myLexer s
  showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]

showTree :: (Show a, Print a) => Verbosity -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

isNumericVal :: Exp -> Bool
isNumericVal EZero     = True
isNumericVal (ESucc t) = isNumericVal t
isNumericVal _         = False

isVal :: Exp -> Bool
isVal ETrue  = True
isVal EFalse = True
isVal t      = isNumericVal t

eval1 :: Exp -> Either EvaluationError Exp
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

-- eval :: Verbosity -> Exp -> IO ()
-- eval v t = case eval1 t of
--   Left  err -> print t
--   Right val -> do
--     putStrLn "Here"
--     case eval1 val of
--       Left  err  -> print t
--       Right nval -> do
--         print val
--         eval v val
--     -- print val
--     -- eval v val

eval :: Verbosity -> Exp -> [Exp]
eval v t = accEval v t [t]
 where
  accEval v t l = case eval1 t of
    Left  err -> l
    Right val -> accEval v val (val : l)
