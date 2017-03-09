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
import           Regiment.Vanguard.IO

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

prop_roundtrip_keyedPayload :: Property
prop_roundtrip_keyedPayload =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (genKP n) $
      binaryTripping bKeyedPayload getKeyedPayload

prop_roundtrip_write_read_line :: Property
prop_roundtrip_write_read_line =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (genKP n) $ \kp ->
      testIO . withTempDirectory "dist" "regiment-test" $ \tmp -> do
        let
          output = tmp </> "out"
        withBinaryFile output WriteMode $ \h -> do
          writeCursor h kp

        withBinaryFile output ReadMode $ \h -> do
          result <- runEitherT $ readCursorIO h
          let
            expected = Right $ NonEmpty h kp

          return $ result === expected

-- TODO: Delete this once end-to-end test is in place
prop_updateMinCursor :: Property
prop_updateMinCursor =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
    gamble (listOf1 (genKP n)) $ \kps ->
      testIO . withTempDirectory "dist" "regiment-test" $ \dir ->
        fmap (either (flip counterexample False) id) . runEitherT $ do
          let
            writeKeys :: Int -> KeyedPayload -> IO FilePath
            writeKeys i ks = do
              let
                f = dir </> show i
              withBinaryFile f WriteMode $ \h -> do
                writeCursor h ks
                pure f

          fs <- liftIO $ zipWithM writeKeys [0..] kps
          mapEitherT runResourceT . firstT show $ do
            handles <- mapM (open ReadMode) fs
            ls <- mapEitherT liftIO $ formVanguardIO handles
            (v, _) <- mapEitherT liftIO $ updateMinCursorIO ls
            case v of
              NonEmpty _ p ->
                return $ p === DL.minimumBy (DO.comparing keys) kps
              EOF ->
                pure $ counterexample "EOF found" False

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
