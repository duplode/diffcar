module Single
    ( subMain
    , Options(..)
    , opts
    ) where

import Print

import qualified Options.Applicative as Opts
import qualified Data.Text.IO as T (putStrLn)

data Options = Options
    { file1 :: FilePath
    , file2 :: FilePath
    , printOpts :: PrintOptions
    }

argFile1 :: Opts.Parser FilePath
argFile1 = Opts.argument Opts.str $
    Opts.help "First CAR*.RES file"
    <> Opts.metavar "FILE-1"

argFile2 :: Opts.Parser FilePath
argFile2 = Opts.argument Opts.str $
    Opts.help "Second CAR*.RES file"
    <> Opts.metavar "FILE-2"

switchPlain :: Opts.Parser Bool
switchPlain = Opts.switch $
    Opts.long "plain"
    <> Opts.help "Output suitable for writing to a text file"

switchHeader :: Opts.Parser Bool
switchHeader = Opts.switch $
    Opts.long "header"
    <> Opts.help "Print header with file names"

switchSkipEquals :: Opts.Parser Bool
switchSkipEquals = Opts.switch $
    Opts.long "skip-equals"
    <> Opts.help "Don't generate output for equal files"

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> argFile1 <*> argFile2
    <*> (PrintOptions
        <$> switchPlain <*> switchHeader <*> switchSkipEquals)

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts $
    Opts.fullDesc
    <> Opts.progDesc "Compare two CAR*.RES files"

subMain :: Options -> IO ()
subMain options = do
    output <- filesDiff
        (printOpts options) (file1 options) (file2 options)
    T.putStrLn output
