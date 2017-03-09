{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Regiment.Arbitrary where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as Boxed

import           P

import           Regiment.Data

import           System.IO (Handle)

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (Jack, oneof, listOf1, listOfN, vectorOf, boundedEnum)
import           Test.QuickCheck.Jack (chooseInt, arbitrary, suchThat)

genBytes :: Jack BS.ByteString
genBytes =
  BS.pack . toList <$> listOf1 boundedEnum

genKey :: Jack Key
genKey =
  Key <$> genBytes

genKP :: Int -> Jack KeyedPayload
genKP numKeys =
  KeyedPayload
    <$> Boxed.fromList <$> (vectorOf numKeys genKey)
    <*> genBytes

genKPNoPayload :: Int -> Jack KeyedPayload
genKPNoPayload numKeys = do
  bs <- vectorOf numKeys genBytes
  ks <- return $ Key <$> bs
  p <- return $ BS.concat bs
  return $
    KeyedPayload {
      keys = Boxed.fromList ks
    , payload = p
    }

genListKPsNoPayload :: Jack [[KeyedPayload]]
genListKPsNoPayload = do
  numKeys <- arbitrary `suchThat` (> 0)
  numLists <- chooseInt (1,10)
  vectorOf numLists $ listOfN 0 numLists (genKPNoPayload numKeys)

genKPsUniqueKeys :: Int -> Int -> Int -> Jack [KeyedPayload]
genKPsUniqueKeys prefix numKeys maxListLength = do
  kps <- listOfN 0 maxListLength (genKP numKeys)
  let
    uniquifier = BSC.pack $ (show prefix) <> "_" <> (show $ length kps)

    prepend :: BS.ByteString -> KeyedPayload -> KeyedPayload
    prepend bs kp =
      KeyedPayload {
          keys = (Key <$> Boxed.singleton bs) Boxed.++ keys kp
        , payload = payload kp }

  return $ prepend uniquifier <$> kps

genListKPsUniqueKeys :: Jack [[KeyedPayload]]
genListKPsUniqueKeys = do
  numKeys <- arbitrary `suchThat` (> 0)
  numLists <- chooseInt (1,10)
  maxListLength <- arbitrary `suchThat` (> 0)
  forM [1 .. numLists] $ \i ->
    genKPsUniqueKeys i numKeys maxListLength

genCursor :: Int -> Handle -> Jack (Cursor Handle)
genCursor n h =
  oneof [
      return EOF
    , NonEmpty <$> return h <*> (genKP n)
  ]
