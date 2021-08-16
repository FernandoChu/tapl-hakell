-- This program takes huge inspiration from the
-- Test.hs program generated from the BNFC Converter

module CommandLine.Cmd where

import qualified Arith.Core                    as Arith
import qualified Arith.Par                      ( pExp )
import           Arith.Print                    ( Print
                                                , printTree
                                                )
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Semigroup                 ( (<>) )
import           Options.Applicative            ( (<**>)
                                                , Parser
                                                , ParserInfo
                                                , auto
                                                , command
                                                , execParser
                                                , flag
                                                , footerDoc
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , strArgument
                                                , subparser
                                                )
import           System.Console.Haskeline       ( defaultSettings
                                                , getInputLine
                                                , runInputT
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import qualified Text.PrettyPrint.ANSI.Leijen  as PrettyDoc
import           Types

data Language = Arith
              | Lambda
  deriving (Read)

data Command = Repl
             | EvalFile FilePath
             | EvalStdin
  deriving (Read)

data CmdOptions = CmdOptions
  { verbosity  :: Verbosity
  , lang       :: Language
  , optCommand :: Command
  }

cmdOptions :: Parser CmdOptions
cmdOptions =
  CmdOptions
    <$> flag Normal
             Verbose
             (long "verbose" <> short 'v' <> help "Verbosely evaluate terms.")
    <*> option
          auto
          (  long "lang"
          <> short 'l'
          <> metavar "LANGUAGE"
          <> help
               "Language of the expressions to evaluate. See below for all available languages."
          )
    <*> subparser (createRepl <> evalFile <> evalStdin)
 where
  createRepl = command
    "repl"
    (info (pure Repl)
          (progDesc "Makes a repl that constantly evaluates user input")
    )

  evalFile = command
    "parse"
    (info parseFile (progDesc "Evaluates de contents of a file"))

  evalStdin = command
    "stdin"
    (info (pure EvalStdin) (progDesc "Evaluates de contents from stdin"))

  parseFile = EvalFile <$> strArgument (metavar "FILE")

mainCmd :: IO ()
mainCmd = do
  opts <- execParser optsParser
  case optCommand opts of
    Repl        -> runRepl (verbosity opts) Arith.Par.pExp
    EvalFile fs -> runFile (verbosity opts) Arith.Par.pExp fs
    EvalStdin   -> getLine >>= run (verbosity opts) Arith.Par.pExp

putStrV :: Verbosity -> String -> IO ()
putStrV v s = case v of
  Normal  -> pure ()
  Verbose -> putStrLn s

runFile
  :: (GExp a, Print a, Show a, GToken tok)
  => Verbosity
  -> ParseFun tok a
  -> FilePath
  -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

runRepl
  :: (GExp a, Print a, Show a, GToken tok)
  => Verbosity
  -> ParseFun tok a
  -> IO ()
runRepl v p = forever $ runInputT defaultSettings $ do
  ml <- getInputLine "% "
  case ml of
    Nothing -> pure ()
    Just l  -> lift $ run v p l

run
  :: (GExp a, Print a, Show a, GToken tok)
  => Verbosity
  -> ParseFun tok a
  -> String
  -> IO ()
run v p s = case p ts of
  Left err -> do
    putStrLn "\nParse              Failed...\n"
    putStrV v "Tokens:"
    mapM_ (putStrV v . showPosToken . gMkPosToken) ts
    putStrV v "Tokens:"
    putStrLn err
  Right tree -> do
    showTree v tree
    forM_ (reverse (eval v tree)) print
 where
  ts = gLexer s
  showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]

showTree :: (Show a, Print a) => Verbosity -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

optsParser :: ParserInfo CmdOptions
optsParser = info
  (cmdOptions <**> helper)
  (  fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header
       (unlines
         [ "Command line for the different interpreters in Benjamin Pierce's Types and"
         , "Programming Languages. Made with Haskell by Ryunaq."
         ]
       )
  <> footerDoc
       (Just
         (PrettyDoc.text
           (unlines
             [ "Available Languages:"
             , "  arith                    Implementation of Chapter 3 language"
             , "  lambda                   Implementation of Chapter 4 language"
             , ""
             , "Examples"
             , "  exe -l=lambda repl   # Starts a repl evaluating untyped lambda expressions"
             , "  exe -v foo.txt       # Evaluates verbosely the contents of the file foo.txt"
             , "  echo \"if true then 0 else succ 0\" | exe -"
             ]
           )
         )
       )
  )
