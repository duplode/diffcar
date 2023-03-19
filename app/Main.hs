module Main where

import Resources

import Data.ByteString.Lazy as B
import Data.Binary.Get

main :: IO ()
main = do
    bin <- B.readFile "data/CARZGT3.RES"
    let ress = runGet getResources bin
    print ress
