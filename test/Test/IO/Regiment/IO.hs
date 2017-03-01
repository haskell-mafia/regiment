{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Regiment.IO where

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.Vector as Boxed

import           Disorder.Core.IO (testIO)

import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, arbitrary)

import           P

import           Regiment.Data
import           Regiment.IO

import           System.IO (IOMode (..), withBinaryFile)
import           System.IO.Temp (withTempDirectory)
import           System.FilePath ((</>))

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, forAll, (===), listOf1)
import           Test.QuickCheck.Jack (Jack, vectorOf, boundedEnum, maxSuccess, stdArgs)

import           X.Control.Monad.Trans.Either (runEitherT)

genBytes :: Jack BS.ByteString
genBytes =
  BS.pack . toList <$> listOf1 boundedEnum

genSortKey :: Jack SortKey
genSortKey =
  SortKey <$> genBytes

genSortKeysWithPayload :: Int -> Jack SortKeysWithPayload
genSortKeysWithPayload n =
  SortKeysWithPayload
    <$> Boxed.fromList <$> (vectorOf n genSortKey)
    <*> (Payload <$> genBytes)

binaryTripping :: (Show a, Eq a) => (a -> Builder) -> Get a -> a -> Property
binaryTripping encode decode =
  let
    third (_, _, x) = x
  in
    tripping
      (Builder.toLazyByteString . encode)
      (bimap third third . Get.runGetOrFail decode)

prop_roundtrip_sortkeyswithpayload :: Property
prop_roundtrip_sortkeyswithpayload =
  forAll (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (genSortKeysWithPayload n) $
      binaryTripping bSortKeysWithPayload getSortKeysWithPayload

prop_roundtrip_write_read_line :: Property
prop_roundtrip_write_read_line =
  forAll (arbitrary `suchThat` (> 0)) $ \n ->
    forAll (genSortKeysWithPayload n) $ \sksp ->
      testIO . withTempDirectory "dist" "regiment-test" $ \tmp -> do
        let
          output = tmp </> "out"
        withBinaryFile output WriteMode $ \h -> do
          writeLine h sksp

        withBinaryFile output ReadMode $ \h -> do
          result <- runEitherT $ readLine h
          let
            expected = Right $ NonEmpty h sksp

          return $ result === expected

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
