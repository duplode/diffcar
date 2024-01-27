{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module: Car
--
-- CAR*.RES resource processing.
module Car
    ( Simd(..)
    , Car(..)
    , StuntsString
    , Point3D
    , Point2D16
    , Point2D8
    , resourcesToCar
    , readCar
    , ppdCarRes
    , showMissingPaths
    ) where

import Resources

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T (pack, unlines)
import Data.Text.Encoding (decodeASCII)
import qualified Data.Text.IO as T (putStrLn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IM
import Data.Binary
import Data.Binary.Get
import Data.Int
import Control.Monad (replicateM)
import Control.Applicative (liftA2, liftA3)
import Control.Selective (Validation(..))
import System.Directory (doesFileExist)

import GHC.Generics (Generic)
import Data.Portray (Portray)
import Data.Portray.Diff (Diff)
import Prettyprinter (Pretty)
import Data.Portray.Prettyprinter (WrappedPortray(..), ppd, showDiff)
import Data.Wrapped (Wrapped(..))

-- | String from Stunts.
--
-- Using Data.ByteString.Char8 would have been more straightforward,
-- but it would require orphan instances of Portray and Diff
type StuntsString = Text

makeStuntsString :: ByteString -> Text
makeStuntsString = decodeASCII . B.toStrict

-- TODO: Ideally, coordinates should be strict tuples.
-- Forgoing that for now, for the sake of expediency.

-- | 3D coordinates (physical model).
type Point3D = (Int16, Int16, Int16)

getPoint3D :: Get Point3D
getPoint3D = liftA3 (,,) getInt16le getInt16le getInt16le

-- | 2D coordinates (dashboard), word-sized.
type Point2D16 = (Word16, Word16)

getPoint2D16 :: Get Point2D16
getPoint2D16 = liftA2 (,) getWord16le getWord16le

-- | 2D coordinates (dashboard), byte-sized.
type Point2D8 = (Word8, Word8)

getPoint2D8 :: Get Point2D8
getPoint2D8 = liftA2 (,) getWord8 getWord8

-- | Create an IntMap from a list of keys and a list of values.
--
-- IntMaps are convenient due to easier displaying, already having
-- Portray and Diff instances, and offering choice of indexing. As
-- far as type accuracy goes, though, the ideal choice would be fixed
-- length vectors.
makeIntMap :: [Int] -> [a] -> IntMap a
makeIntMap ks = IM.fromList . zip ks

-- | Representation of the simd resource.
data Simd = Simd
    { numGears :: !Word8
    , purpleByte :: !Word8
    , carMass :: !Word16
    , brakingEff :: !Word16
    , idleRpm :: !Word16
    , downshiftRpm :: !Word16
    , upshiftRpm :: !Word16
    , maximumRpm :: !Word16
    , yellowWord :: !Word16
    , gearRatios :: !(IntMap Word16)
    , knobUnknown :: !Word16
    , knobCentre :: !Word16
    , knobPositions :: !(IntMap Point2D16)
    , aeroResistance :: !Word16
    , idleRpmTorque :: !Word8
    , torqueCurve :: !(IntMap Word8)
    , blueWord :: !Word16
    , baseGrip :: !Word16
    , redWordsA :: !(IntMap Word16)
    , redWordNeedleColour :: !Word16
    , redWordsB :: !(Word16, Word16)
    , airGrip :: !Word16
    , asphaltGrip :: !Word16
    , dirtGrip :: !Word16
    , iceGrip :: !Word16
    , grassGrip :: !Word16
    , orangeWords :: !(IntMap Word16)
    , tracksideHalfWidth :: !Word16
    , tracksideHalfHeight :: !Word16
    , tracksideHalfLength :: !Word16
    , tracksideCutoffRadius :: !Word16
    , insideCarHeight :: !Word16
    , wheelCoords :: !(IntMap Point3D)
    , steeringDotCentre :: !(Word8, Word8)
    , steeringDotArc :: !(IntMap Point2D8)
    , speedometerCentre :: !Point2D16
    , speedometerNumPositions :: !Word16
    , speedometerArc :: !(IntMap Point2D8)
    , tachometerCentre :: !Point2D16
    , tachometerNumPositions :: !Word16
    , tachometerArc :: !(IntMap Point2D8)
    }
    deriving (Eq, Show, Generic)
    deriving (Portray, Diff) via Wrapped Generic Simd
    deriving Pretty via WrappedPortray Simd

getSimd :: Get Simd
getSimd = do
    numGears <- getWord8
    purpleByte <- getWord8
    carMass <- getWord16le
    brakingEff <- getWord16le
    idleRpm <- getWord16le
    downshiftRpm <- getWord16le
    upshiftRpm <- getWord16le
    maximumRpm <- getWord16le
    yellowWord <- getWord16le
    gearRatios <- makeIntMap [1..] <$> replicateM 6 getWord16le
    knobUnknown <- getWord16le
    knobCentre <- getWord16le
    knobPositions <- makeIntMap [1..] <$> replicateM 6 getPoint2D16
    aeroResistance <- getWord16le
    idleRpmTorque <- getWord8
    torqueCurve <- makeIntMap [0, 128..] <$> replicateM 103 getWord8
    blueWord <- getWord16le
    baseGrip <- getWord16le
    redWordsA <- makeIntMap [1..] <$> replicateM 4 getWord16le
    redWordNeedleColour <- getWord16le
    redWordsB <- liftA2 (,) getWord16le getWord16le
    airGrip <- getWord16le
    asphaltGrip <- getWord16le
    dirtGrip <- getWord16le
    iceGrip <- getWord16le
    grassGrip <- getWord16le
    orangeWords <- makeIntMap [1..] <$> replicateM 5 getWord16le
    tracksideHalfWidth <- getWord16le
    tracksideHalfHeight <- getWord16le
    tracksideHalfLength <- getWord16le
    tracksideCutoffRadius <- getWord16le
    insideCarHeight <- getWord16le
    wheelCoords <- makeIntMap [1..] <$> replicateM 4 getPoint3D
    steeringDotCentre <- getPoint2D8
    steeringDotArc <- makeIntMap [1..] <$> replicateM 30 getPoint2D8
    speedometerCentre <- getPoint2D16
    speedometerNumPositions <- getWord16le
    speedometerArc <- makeIntMap [0, 25..] <$> replicateM 104 getPoint2D8
    tachometerCentre <- getPoint2D16
    tachometerNumPositions <- getWord16le
    tachometerArc <- makeIntMap [0, 128..] <$> replicateM 128 getPoint2D8
    return $! Simd {..}

-- | Strongly typed representation of CAR*.RES.
data Car = Car
    { edes :: !StuntsString
    , gnam :: !StuntsString
    , gsna :: !StuntsString
    , simd :: !Simd
    }
    deriving (Eq, Show, Generic)
    deriving (Portray, Diff) via Wrapped Generic Car
    deriving Pretty via WrappedPortray Car

-- | Makes a car out of untyped resources.
resourcesToCar :: Resources -> Car
resourcesToCar ress = Car {..}
    where
    -- TODO: No protection against missing resources.
    edes = makeStuntsString (ress ! "edes")
    gnam = makeStuntsString (ress ! "gnam")
    gsna = makeStuntsString (ress ! "gsna")
    simd = runGet getSimd (ress ! "simd")

-- | Makes a car by reading a CAR*.RES file.
readCar :: FilePath -> IO (Validation [FilePath] Car)
readCar path = do
    exists <- doesFileExist path
    -- This would be traverse if this Validation had the instance.
    vBin <- if exists
        then Success <$> B.readFile path
        else return $ Failure [path]
    return $! (resourcesToCar . runGet getResources) <$> vBin

-- | Pretty-print to console the differences between CAR*.RES files.
-- Primarily meant for debugging.
ppdCarRes :: Bool -> FilePath -> FilePath -> IO ()
ppdCarRes plain path1 path2 = do
    vCar1 <- readCar path1
    vCar2 <- readCar path2
    case liftA2 (,) vCar1 vCar2 of
        Failure ps -> T.putStrLn $ showMissingPaths ps
        Success (car1, car2) -> if plain
            then T.putStrLn (showDiff car1 car2)
            else ppd car1 car2

-- | Formats a list of missing paths from readCar for display.
showMissingPaths :: [FilePath] -> Text
showMissingPaths ps = T.unlines $ "Missing files:" : map T.pack ps

