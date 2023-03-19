module Main where

import Car
import qualified Options.Applicative as Opts

data Options = Options
    { file1 :: FilePath
    , file2 :: FilePath
    , plain :: Bool
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

baseOpts :: Opts.Parser Options
baseOpts = Options <$> argFile1 <*> argFile2 <*> switchPlain

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts $
    Opts.fullDesc
    <> Opts.progDesc "Compare two CAR*.RES files"

main :: IO ()
main = do
    options <- Opts.execParser opts
    ppdCarRes (plain options) (file1 options) (file2 options)
