module CommandLine.Cmd where

import           Arith.Abs
import qualified Arith.Core                    as Arith
import           Arith.Lex
import           Arith.Par
import           Arith.Print
import           Arith.Skel
import           Control.Monad                  ( forever )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Semigroup                 ( (<>) )
import           Options.Applicative
import           System.Console.Haskeline
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
             | EvalFile String
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
    Repl -> forever $ runInputT defaultSettings $ do
      ml <- getInputLine "% "
      case ml of
        Nothing -> pure ()
        Just l  -> lift $ Arith.run (verbosity opts) pExp l
    EvalFile a -> greet opts
    EvalStdin  -> greet opts
 where
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

greet :: CmdOptions -> IO ()
greet (CmdOptions h l n) = putStrLn $ "Hello, " ++ "!"
-- data Language
--   = Arith
--   | Lambda
--   deriving (Show)
-- data Command
--   = Repl
--   | EvalFile
--   | EvalStdin
--   deriving (Show)
-- updateVerbosity :: CmdOptions -> Verbosity -> CmdOptions
-- updateVerbosity cmdopt y = cmdopt {verbosity = y}
-- updateLang :: CmdOptions -> Language -> CmdOptions
-- updateLang cmdopt y = cmdopt {lang = y}
-- updateCommand :: CmdOptions -> Command -> CmdOptions
-- updateCommand cmdopt y = cmdopt {command = y}
-- updateHelp :: CmdOptions -> Bool -> CmdOptions
-- updateHelp cmdopt y = cmdopt {help = y}
-- usage :: IO ()
-- usage = do
--   putStrLn $
--     unlines
--       [ "usage: exe [option]... [command]",
--         "Command line for the different interpreters in Benjamin Pierce's Types and",
--         "Programming Languages. Made with Haskell by Ryunaq.",
--         "",
--         "Available options:",
--         "  -h, --help      Display this help message.",
--         "  -l, --lang      Language implementation to use. See available ones below",
--         "  -v, --verbose   Display parse and evaluation steps",
--         "",
--         "Available implementations:",
--         "  arith           Implementation of Chapter 3 language",
--         "",
--         "Available commands:",
--         "  repl            Makes a repl that constantly evaluates user input",
--         "  eval <file>     Evaluates de contents of a file",
--         "  -               Evaluates de contents from stdin",
--         "",
--         "Examples",
--         "  exe -l=lambda repl   # Starts a repl evaluating untyped lambda expressions",
--         "  exe -v foo.txt       # Evaluates verbosely the contents of the file foo.txt",
--         "  echo \"if true then 0 else succ 0\" | exe -"
--       ]
--   exitFailure
-- defaultOptions = CmdOptions {verbosity = 0, lang = Arith, command = Repl, help = False}
-- parseOptions :: [String] -> Either String CmdOptions
-- parseOptions x = foldr parseOptions (Right defaultOptions) (reverse x)
--   where
--     parseOptions str eitherOptions = do
--       options <- eitherOptions
--       case str of
--         "-v"        -> Right $ updateVerbosity options 1
--         "--verbose" -> Right $ updateVerbosity options 1
--         "-h"        -> Right $ updateVerbosity options 1
--         "--help"    -> Right $ updateVerbosity options 1
--         _           -> Left "Can't parse"
-- cmd :: IO ()
-- cmd = do
--   args <- getArgs
--   case args of
--     "--help":_ -> usage
--     x:xs       ->    getContents >>= run 2 pExp
--   -- args <- getArgs
--   -- case args of
--   --   "--help":_          -> usage
--   --   x:xs             -> getContents >>= run 2 pExp
--   --   "arith" : "-s" : fs -> mapM_ (runFile 0 pExp) fs
--   --   fs                  -> mapM_ (runFile 2 pExp) fs
