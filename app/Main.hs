{-# LANGUAGE OverloadedStrings #-}
module Main where

import Car
import qualified Options.Applicative as Opts
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import Control.Monad (when)

data Options = Options
    { file1 :: FilePath
    , file2 :: FilePath
    , plain :: Bool
    , header :: Bool
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

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> argFile1 <*> argFile2
    <*> switchPlain <*> switchHeader

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts $
    Opts.fullDesc
    <> Opts.progDesc "Compare two CAR*.RES files"

main :: IO ()
main = do
    options <- Opts.execParser opts
    let p1 = file1 options
        p2 = file2 options
    when (header options) $ T.putStrLn (headerText p1 p2)
    ppdCarRes (plain options) p1 p2
    when (header options) $ T.putStrLn ""
    where
    headerText p1 p2 = T.unlines
        [ T.replicate 72 "-"
        , T.pack p1
        , T.pack p2
        , T.replicate 72 "-"
        ]
