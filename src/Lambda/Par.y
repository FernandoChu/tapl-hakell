-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Lambda.Par
  ( happyError
  , myLexer
  , pExp
  ) where

import Prelude

import qualified Lambda.Abs
import Lambda.Lex

}

%name pExp Exp
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '0'     { PT _ (TS _ 1) }
  'else'  { PT _ (TS _ 2) }
  'false' { PT _ (TS _ 3) }
  'if'    { PT _ (TS _ 4) }
  'pred'  { PT _ (TS _ 5) }
  'succ'  { PT _ (TS _ 6) }
  'then'  { PT _ (TS _ 7) }
  'true'  { PT _ (TS _ 8) }

%%


Exp :: { Lambda.Abs.Exp }
Exp
  : 'true' { Lambda.Abs.ETrue }
  | 'false' { Lambda.Abs.EFalse }
  | 'if' Exp 'then' Exp 'else' Exp { Lambda.Abs.ECond $2 $4 $6 }
  | '0' { Lambda.Abs.EZero }
  | 'succ' Exp { Lambda.Abs.ESucc $2 }
  | 'pred' Exp { Lambda.Abs.EPred $2 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}
