{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Single

import qualified Options.Applicative as Opts
import Control.Applicative

main :: IO ()
main = do
    fullOpts <- Opts.customExecParser p outerOpts
    case fullOpts of
        Single o -> Single.subMain o
    where
    p = Opts.prefs (Opts.showHelpOnError <> Opts.showHelpOnEmpty)

data Command = Single Single.Options

outerOpts :: Opts.ParserInfo Command
outerOpts = Opts.info (commandOpts <**> Opts.helper <**> optVersion)
    ( Opts.fullDesc
    <> Opts.progDesc "Take Stunts-aware diffs of CAR*.RES files"
    )
    where
    commandOpts = Opts.hsubparser
        ( Opts.command "single" (Single <$> Single.opts)
        )
    -- TODO: Get the version from cabal metatdata.
    optVersion = Opts.infoOption "diffcar 0.1"
        (Opts.long "version" <> Opts.help "Print version information")
