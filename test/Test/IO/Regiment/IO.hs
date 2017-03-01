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
import qualified Data.Vector as Boxed

import           Disorder.Core.IO (testIO)

import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, arbitrary)

import           P

import           Regiment.Data
import           Regiment.IO

import           System.IO (FilePath, IO, IOMode (..), withBinaryFile)
import           System.IO.Temp (withTempDirectory)
import           System.FilePath ((</>))

import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, forAll, (===))
import           Test.QuickCheck.Jack (maxSuccess, stdArgs, counterexample)

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

prop_updateMinLine :: Property
prop_updateMinLine =
  forAll (arbitrary `suchThat` (> 0)) $ \n ->
  forAll (arbitrary `suchThat` (>= 0)) $ \(k :: Int)  ->
  forAll (for [0..k] $ \_ -> genSortKeysWithPayload n) $ \sksps ->
  testIO . withTempDirectory "dist" "regiment-test" $ \dir ->
    fmap (either (flip counterexample False) id) . runEitherT $ do
      let
        writeSortKeys :: Int -> SortKeysWithPayload -> IO FilePath
        writeSortKeys i sksp = do
          let f = dir </> show i
          withBinaryFile f WriteMode $ \h ->
            writeLine h sksp
          pure f
      fs <- liftIO $ for (DL.zip [0..] (sksps)) (uncurry writeSortKeys)
      mapEitherT runResourceT . firstT show $ do
        ls <- constructLines fs
        (l, _) <- mapEitherT liftIO $ updateMinLine ls
        case l of
          NonEmpty _ p ->
            pure $ p === minBySortKeys sksps
          EOF ->
            pure $ counterexample "EOF found" False

minBySortKeys :: [SortKeysWithPayload] -> SortKeysWithPayload
minBySortKeys sksps =
  -- assumes non-empty list
  sksps DL.!! Boxed.minIndex (sortKeys <$> (Boxed.fromList sksps))

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
