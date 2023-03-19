{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module: Car
--
-- CAR*.RES resource processing.
module Car where

import Resources

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Binary
import Data.Binary.Get

-- | Raw C String from Stunts.
type StuntsString  = B8.ByteString

-- | Representation of the simd resource.
type Simd = ()

-- | Strongly typed representation of CAR*.RES.
data Car = Car
    { edes :: StuntsString
    , gnam :: StuntsString
    , gsna :: StuntsString
    , simd :: Simd
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
    simd = ()
