{-# LANGUAGE OverloadedStrings #-}
module Print
    ( filesDiff
    , diffHeader
    , PrintOptions(..)
    ) where

import Car

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Maybe (isNothing)
import qualified Control.Selective as Sel
import qualified Data.Portray.Diff as Portray
import qualified Data.Portray.Prettyprinter as Portray
import System.IO (stdout)
import System.Console.ANSI (hNowSupportsANSI)

data PrintOptions = PrintOptions
    { plain :: Bool
    , header :: Bool
    , skipEquals :: Bool
    }

filesDiff :: PrintOptions -> FilePath -> FilePath -> IO T.Text
filesDiff options path1 path2 = do
    supportsPretty <- hNowSupportsANSI stdout
    let headerText = diffHeader path1 path2
        chosenShow = if plain options || not (supportsPretty)
            then Portray.basicShowPortrayal
            else TL.toStrict . Portray.prettyShowPortrayalLazy
    vCar1 <- readCar path1
    vCar2 <- readCar path2
    let vDiffCar = Portray.diff <$> vCar1 <*> vCar2
    case vDiffCar of
        Sel.Failure ps -> return $ T.unlines
            [ if header options then headerText else mempty
            , showMissingPaths ps
            ]
        Sel.Success diffCar -> return $
            if skipEquals options && isNothing diffCar
                then mempty
                -- TODO: Deduplicate.
                else T.unlines
                    [ if header options then headerText else mempty
                    , maybe "_" chosenShow diffCar
                    ]

diffHeader :: FilePath -> FilePath -> T.Text
diffHeader path1 path2 =  T.unlines
    [ T.replicate 72 "-"
    , T.pack path1
    , T.pack path2
    , T.replicate 72 "-"
    ]
