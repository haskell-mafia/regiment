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
import qualified Data.ByteString as BS
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
import           Regiment.Parse
import           Regiment.Serial
import           Regiment.Vanguard.IO

import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           System.IO (FilePath, IO, Handle, IOMode (..), hIsEOF, hClose, withBinaryFile)
import           System.IO.Temp (withTempFile, withTempDirectory)

import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, (===))
import           Test.QuickCheck.Jack (vectorOf, maxSuccess, stdArgs, counterexample, listOf1)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT, mapEitherT)


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

prop_roundtrip_write_read_sorted_tmp_file =
  gamble (arbitrary `suchThat` (> 0)) $ \n ->
  gamble (arbitrary `suchThat` (\f -> (formatColumnCount f) > 0)) $ \fmt ->
  gamble (genListSortColumns fmt) $ \sc ->
  gamble (vectorOf n $ genRealKP fmt sc) $ \kps ->
    testIO . withTempFile "dist" "test-input-" $ \tmpFile hFile -> do
      -- write rs to InputFile
      -- note that our payloads are terminated by newlines so no need to
      -- add terminating newline
      BS.hPut hFile $ BS.concat (payload <$> kps)
      hClose hFile
      -- sort the rs in memory
      let expected = payload <$> DL.sort kps

      -- toTempFiles on the input file that was just created
      withTempDirectory "dist" "regiment-test" $ \tmp -> do
        success <- runEitherT $ toTempFiles (InputFile tmpFile) (TempDirectory tmp) fmt sc (10 * 1024)
        case success of
          Left e ->
            return $ counterexample ("toTempFiles errored out: " <> show e) False
          Right _ -> do
            -- read the contents of the sorted temp files
            tmpFilePaths <- fmap (filter (flip notElem [".", ".."])) $ getDirectoryContents tmp
            mresult <- runEitherT . readPayloads $ fmap (tmp </>) tmpFilePaths
            return $ case mresult of
              Left _ -> counterexample "RegimentIOError" False
              Right result -> expected === result

slurp :: Handle -> [BS.ByteString] -> EitherT RegimentMergeIOError IO [BS.ByteString]
slurp h ps = do
  isEOF <- liftIO $ hIsEOF h
  if isEOF
    then
      return ps
    else do
      mp <- readKeyedPayloadIO h
      case mp of
        Nothing -> slurp h ps
        Just p -> slurp h (ps DL.++ [payload p])

readPayloads :: [FilePath] -> EitherT RegimentMergeIOError IO [BS.ByteString]
readPayloads fps = do
  let
    f :: FilePath -> EitherT RegimentMergeIOError IO [BS.ByteString]
    f fp = newEitherT $ do
      withBinaryFile fp ReadMode $ \h
        -> runEitherT $ slurp h []

  lps' <- mapM f fps
  return $ concat lps'

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
