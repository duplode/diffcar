module Main where

import qualified Single
import qualified Report

import qualified Options.Applicative as Opts
import Control.Applicative
import Paths_diffcar (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    fullOpts <- Opts.customExecParser p outerOpts
    case fullOpts of
        Single o -> Single.subMain o
        Report o -> Report.subMain o
    where
    p = Opts.prefs (Opts.showHelpOnError <> Opts.showHelpOnEmpty)

data Command
    = Single Single.Options
    | Report Report.Options

outerOpts :: Opts.ParserInfo Command
outerOpts = Opts.info (commandOpts <**> Opts.helper <**> optVersion)
    ( Opts.fullDesc
    <> Opts.progDesc "Take Stunts-aware diffs of CAR*.RES files"
    )
    where
    commandOpts = Opts.hsubparser
        ( Opts.command "single" (Single <$> Single.opts)
        <> Opts.command "report" (Report <$> Report.opts)
        )
    optVersion = Opts.infoOption ("diffcar " ++ showVersion version)
        (Opts.long "version" <> Opts.help "Print version information")
