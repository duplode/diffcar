{-# LANGUAGE OverloadedStrings #-}
module Single
    ( subMain
    , Options(..)
    , opts
    ) where

import Car

import qualified Options.Applicative as Opts
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Text.Lazy as TL (toStrict)
import Control.Monad (when, unless)
import Data.Maybe (isNothing)
import qualified Control.Selective as Sel
import qualified Data.Portray.Diff as Portray
import qualified Data.Portray.Prettyprinter as Portray

data Options = Options
    { file1 :: FilePath
    , file2 :: FilePath
    , plain :: Bool
    , header :: Bool
    , skipEquals :: Bool
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
    <*> switchPlain <*> switchHeader <*> switchSkipEquals

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts $
    Opts.fullDesc
    <> Opts.progDesc "Compare two CAR*.RES files"

subMain :: Options -> IO ()
subMain options = do
    let path1 = file1 options
        path2 = file2 options
    let headerText = T.unlines
            [ T.replicate 72 "-"
            , T.pack path1
            , T.pack path2
            , T.replicate 72 "-"
            ]
        chosenShow = if plain options
            then Portray.basicShowPortrayal
            else TL.toStrict . Portray.prettyShowPortrayalLazy
    vCar1 <- readCar path1
    vCar2 <- readCar path2
    let vDiffCar = Portray.diff <$> vCar1 <*> vCar2
    case vDiffCar of
        Sel.Failure ps -> do
            when (header options) $ T.putStrLn headerText
            T.putStrLn $ showMissingPaths ps
        Sel.Success diffCar ->
            unless (skipEquals options && isNothing diffCar) $ do
                when (header options) $ T.putStrLn headerText
                T.putStrLn (maybe "_" chosenShow diffCar)
                when (header options) $ T.putStrLn ""
