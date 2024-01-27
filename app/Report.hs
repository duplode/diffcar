module Report
    ( subMain
    , Options(..)
    , opts
    ) where

import qualified Print

import qualified Options.Applicative as Opts
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL (putStrLn, writeFile)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import Data.Char (toUpper)
import Data.List (sortBy)
import Data.Ord (comparing)

data Options = Options
    { dir1 :: FilePath
    , dir2 :: FilePath
    , outputFile :: Maybe FilePath
    , skipEquals :: Bool
    }

argDir1 :: Opts.Parser FilePath
argDir1 = Opts.argument Opts.str $
    Opts.help "First directory"
    <> Opts.metavar "DIR-1"

argDir2 :: Opts.Parser FilePath
argDir2 = Opts.argument Opts.str $
    Opts.help "Second directory"
    <> Opts.metavar "DIR-2"

outputFileOption :: Opts.Parser (Maybe FilePath)
outputFileOption = Opts.option (Just <$> Opts.str)
    ( Opts.short 'o'
    <> Opts.long "output-file"
    <> Opts.help "Output file (prints to terminal/stdout if omitted)"
    <> Opts.metavar "[FILE]"
    <> Opts.value Nothing
    )

switchSkipEquals :: Opts.Parser Bool
switchSkipEquals = Opts.switch $
    Opts.long "skip-equals"
    <> Opts.help "Don't generate output for equal files"

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> argDir1 <*> argDir2 <*> outputFileOption <*> switchSkipEquals

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts $
    Opts.fullDesc
    <> Opts.progDesc "Compare two directories, pairing CAR*.RES files with matching names"

subMain :: Options -> IO ()
subMain options = do
    let printOpts = Print.PrintOptions
            { Print.header = True
            , Print.plain = True
            , Print.skipEquals = skipEquals options
            }
        outAction = maybe TL.putStrLn TL.writeFile (outputFile options)
    pairs <- pairTargets (dir1 options) (dir2 options)
    output <- prepareReport printOpts pairs
    outAction $ TL.toLazyText output

pairTargets :: FilePath -> FilePath -> IO [(FilePath, FilePath)]
pairTargets dir1' dir2' = do
    needleNames <- arrangeNames <$> getDirectoryContents dir1'
    targetNames <- arrangeNames <$> getDirectoryContents dir2'
    let pairs = makePairs [] needleNames targetNames
    return $ map (\(n, t) -> (dir1' </> n, dir2' </> t)) pairs
    where
    isCarRes name = map toUpper (take 3 name) == "CAR"
        && map toUpper (takeExtension name) == ".RES"
    -- TODO: The matching logic shouldn't be done again in Car.readCar
    arrangeNames = sortBy (comparing (map toUpper)) . filter isCarRes
    dropUntilMatch needle = dropWhile ((map toUpper needle /=) . map toUpper)
    makePairs pairs _ [] = pairs
    makePairs pairs [] _ = pairs
    makePairs pairs (n : ns) targets =
        case dropUntilMatch n targets of
            [] -> makePairs pairs ns targets
            t : ts -> (n, t) : makePairs pairs ns ts

prepareReport :: Print.PrintOptions -> [(FilePath, FilePath)] -> IO TL.Builder
prepareReport printOpts = foldMap $ \(n, t) ->
    TL.fromText <$> Print.filesDiff printOpts n t
