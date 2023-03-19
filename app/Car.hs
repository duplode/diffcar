{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: Car
--
-- CAR*.RES resource processing.
module Car where

import Resources

import qualified Data.ByteString.Char8 as B8
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Binary
import Data.Binary.Get
import Data.Int
import Control.Monad (replicateM)
import Control.Applicative (liftA2, liftA3)

-- | Raw C String from Stunts.
type StuntsString = B8.ByteString

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
    , gearRatios :: !(Vector Word16)
    , knobUnknown :: !Word16
    , knobCentre :: !Word16
    , knobPositions :: !(Vector Point2D16)
    , aeroResistance :: !Word16
    , idleRpmTorque :: !Word8
    , torqueCurve :: !(Vector Word8)
    , blueWord :: !Word16
    , baseGrip :: !Word16
    , redWordsA :: !(Vector Word16)
    , redWordNeedleColour :: !Word16
    , redWordsB :: !(Word16, Word16)
    , airGrip :: !Word16
    , asphaltGrip :: !Word16
    , dirtGrip :: !Word16
    , iceGrip :: !Word16
    , grassGrip :: !Word16
    , orangeWords :: !(Vector Word16)
    , tracksideHalfWidth :: !Word16
    , tracksideHalfHeight :: !Word16
    , tracksideHalfLength :: !Word16
    , tracksideCutoffRadius :: !Word16
    , insideCarHeight :: !Word16
    , wheelCoords :: !(Vector Point3D)
    , steeringDotCentre :: !(Word8, Word8)
    , steeringDotArc :: !(Vector Point2D8)
    , speedometerCentre :: !Point2D16
    , speedometerNumPositions :: !Word16
    , speedometerArc :: !(Vector Point2D8)
    , tachometerCentre :: !Point2D16
    , tachometerNumPositions :: !Word16
    , tachometerArc :: !(Vector Point2D8)
    }
    deriving (Eq, Show)

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
    gearRatios <- V.fromList <$> replicateM 6 getWord16le
    knobUnknown <- getWord16le
    knobCentre <- getWord16le
    knobPositions <- V.fromList <$> replicateM 6 getPoint2D16
    aeroResistance <- getWord16le
    idleRpmTorque <- getWord8
    torqueCurve <- V.fromList <$> replicateM 103 getWord8
    blueWord <- getWord16le
    baseGrip <- getWord16le
    redWordsA <- V.fromList <$> replicateM 4 getWord16le
    redWordNeedleColour <- getWord16le
    redWordsB <- liftA2 (,) getWord16le getWord16le
    airGrip <- getWord16le
    asphaltGrip <- getWord16le
    dirtGrip <- getWord16le
    iceGrip <- getWord16le
    grassGrip <- getWord16le
    orangeWords <- V.fromList <$> replicateM 5 getWord16le
    tracksideHalfWidth <- getWord16le
    tracksideHalfHeight <- getWord16le
    tracksideHalfLength <- getWord16le
    tracksideCutoffRadius <- getWord16le
    insideCarHeight <- getWord16le
    wheelCoords <- V.fromList <$> replicateM 4 getPoint3D
    steeringDotCentre <- getPoint2D8
    steeringDotArc <- V.fromList <$> replicateM 30 getPoint2D8
    speedometerCentre <- getPoint2D16
    speedometerNumPositions <- getWord16le
    speedometerArc <- V.fromList <$> replicateM 104 getPoint2D8
    tachometerCentre <- getPoint2D16
    tachometerNumPositions <- getWord16le
    tachometerArc <- V.fromList <$> replicateM 128 getPoint2D8
    return $! Simd {..}

-- | Strongly typed representation of CAR*.RES.
data Car = Car
    { edes :: !StuntsString
    , gnam :: !StuntsString
    , gsna :: !StuntsString
    , simd :: !Simd
    }
    deriving (Eq, Show)

-- | Makes a car out of untyped resources.
resourcesToCar :: Resources -> Car
resourcesToCar ress = Car {..}
    where
    -- TODO: No protection against missing resources.
    edes = B8.toStrict (ress ! "edes")
    gnam = B8.toStrict (ress ! "gnam")
    gsna = B8.toStrict (ress ! "gsna")
    simd = runGet getSimd (ress ! "simd")

