module Main where

import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import           Arith.Abs
import           Arith.Core
import           Arith.Lex
import           Arith.Par
import           Arith.Print
import           Arith.Skel
import           CommandLine.Cmd
import           Lib


-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [              "--help"]  -> usage
--     [              arith   ]  -> getContents >>= run 2 pExp
--     "arith" : "-s" :       fs -> mapM_ (runFile 0 pExp) fs
--     fs                        -> mapM_ (runFile 2 pExp) fs

main :: IO ()
main = mainCmd


{- |
>>> parseOptions $ ["-v"]
<BLANKLINE>
ByteCodeLink.lookupCE
During interactive linking, GHCi couldn't find the following symbol:
  taplzmhaskellzm0zi1zi0zi0zmEvVbehdKJJtJ0KvZZWJQdvv_CommandLineziCmd_zdfShowCmdOptions_closure
This may be due to you not asking GHCi to load extra object files,
archives or DLLs needed by your current session.  Restart GHCi, specifying
the missing library using the -L/path/to/object/dir and -lmissinglibname
flags, or simply by naming the relevant files on the GHCi command line.
Alternatively, this link failure might indicate a bug in GHCi.
If you suspect the latter, please report this as a GHC bug:
  https://www.haskell.org/ghc/reportabug
-}
