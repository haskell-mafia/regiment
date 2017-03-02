{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Regiment.IO where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.List as DL
import qualified Data.Ord as DO

import           Disorder.Core.IO (testIO)

import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, arbitrary)

import           P

import           Regiment.Data
import           Regiment.IO
import           Regiment.Vanguard

import           System.IO (FilePath, IO, IOMode (..), withBinaryFile)
import           System.IO.Temp (withTempDirectory)
import           System.FilePath ((</>))

import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, (===))
import           Test.QuickCheck.Jack (maxSuccess, stdArgs, counterexample, listOf1)

import           X.Control.Monad.Trans.Either (runEitherT, mapEitherT)


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
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (genSortKeysWithPayload n) $
      binaryTripping bSortKeysWithPayload getSortKeysWithPayload

prop_roundtrip_write_read_line :: Property
prop_roundtrip_write_read_line =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (genSortKeysWithPayload n) $ \sksp ->
      testIO . withTempDirectory "dist" "regiment-test" $ \tmp -> do
        let
          output = tmp </> "out"
        withBinaryFile output WriteMode $ \h -> do
          writeCursor h sksp

        withBinaryFile output ReadMode $ \h -> do
          result <- runEitherT $ readCursor h
          let
            expected = Right $ NonEmpty h sksp

          return $ result === expected

prop_updateMinCursor :: Property
prop_updateMinCursor =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (listOf1 (genSortKeysWithPayload n)) $ \sksps ->
      testIO . withTempDirectory "dist" "regiment-test" $ \dir ->
        fmap (either (flip counterexample False) id) . runEitherT $ do
          let
            writeSortKeys :: Int -> SortKeysWithPayload -> IO FilePath
            writeSortKeys i sksp = do
              let
                f = dir </> show i
              withBinaryFile f WriteMode $ \h -> do
                writeCursor h sksp
                pure f

          fs <- liftIO $ zipWithM writeSortKeys [0..] sksps
          mapEitherT runResourceT . firstT show $ do
            ls <- formVanguard fs
            (v, _) <- mapEitherT liftIO $ updateMinCursor ls
            case v of
              NonEmpty _ p ->
                return $ p === DL.minimumBy (DO.comparing sortKeys) sksps
              EOF ->
                pure $ counterexample "EOF found" False

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
