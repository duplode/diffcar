{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Resource
--
-- Unpacked resource file processing.
module Resources
    ( ResId
    , Resources
    , (!)
    , getResources
    ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Binary
import Data.Binary.Get
import Control.Monad (replicateM, foldM)
import Data.Ord (comparing)
import Data.List (sortBy)

-- | Resource ids. Four 8-bit characters (not enforced yet).
type ResId = B8.ByteString

-- | Weakly typed representation of resource file data.
newtype Resources = Resources
    { unResources :: Map ResId ByteString
    } deriving (Eq, Show)

-- | Given a resource id, retrieves the raw data (partial).
(!) :: Resources -> ResId -> ByteString
ress ! rid = M.findWithDefault (error errMsg) rid (unResources ress)
    where
    errMsg = "Resources.!: resource " ++ B8.unpack rid ++ " not found"

-- | Read resources from a file as raw bytestrings.
getResources :: Get Resources
getResources = do
    -- Declared file length (skip)
    fileLen <- fromIntegral <$> getWord32le
    -- Declared number of resources
    nRes <- fromIntegral <$> getWord16le
    -- Resource ids and offsets
    resIds <- replicateM nRes (getByteString 4)
    offsets <- replicateM nRes (fromIntegral <$> getWord32le)
    let headerLen = 6 + 8 * nRes
        finalOffset = fileLen - headerLen
        resSpecs = prepareResSpecs finalOffset (zip resIds offsets)
    Resources <$> foldM bringRes M.empty resSpecs
    where
    calcResLengths fo rios = zipWith (\(rid, o) (_, o') -> (rid, o' - o))
            rios
            (drop 1 rios ++ [("", fo)])
    prepareResSpecs fo = calcResLengths fo . sortBy (comparing snd)
    bringRes ress (rid, len) = do
        res <- getLazyByteString (fromIntegral len)
        return $! M.insert rid res ress


