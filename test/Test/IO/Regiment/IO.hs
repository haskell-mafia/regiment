{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Regiment.IO where

import           Control.Monad.IO.Class (liftIO)

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.List as DL
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)

import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, arbitrary)

import           P

import           Regiment.Data
import           Regiment.IO
import           Regiment.Parse
import           Regiment.Serial
import           Regiment.Vanguard.IO

import           System.Exit
import           System.FilePath ((</>), takeBaseName)
import           System.IO (FilePath, IO, Handle, IOMode (..), hIsEOF, hClose, withBinaryFile)
import           System.IO.Temp (withTempDirectory, openTempFile)
import           System.Process (readProcessWithExitCode, createProcess, proc, waitForProcess, env)

import           Test.Parsley.Arbitrary
import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, property, forAllProperties, quickCheckWithResult)
import           Test.QuickCheck.Jack (listOfN, maxSuccess, stdArgs, counterexample, (===), mkJack_)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)


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

prop_regiment =
  gamble genRestrictedFormat $ \fmt ->
  gamble (genSortColumns fmt) $ \sc ->
  gamble (listOfN 1 100 $ (mkJack_ $ genRow fmt) `suchThat` (not . BS.null)) $ \rs -> testIO $ do
    withTempDirectory "dist" "regiment-test." $ \tmp -> do
      tmpFile <- writeToTmpFile tmp fmt rs
      success <- runEitherT $ regiment (InputFile tmpFile)
                                       (Just . OutputFile $ tmp </> "regiment-sorted")
                                       sc
                                       (formatKind fmt)
                                       (formatNewline fmt)
                                       (NumColumns (formatColumnCount fmt))
                                       (formatSeparator fmt)
                                       (MemoryLimit (1024 * 1024))
      case success of
        Left e ->
          return $ counterexample ("regiment errored out: " <> show e) False
        Right _ -> do
          let
            -- below is to end up with key options for sort so that
            -- e.g. sort cols of 2, 4 correspond to
            -- "-k", "2,2", "-k", "4,4"
            sc' = ((\k -> k + 1) . sortColumn) <$> sc
            scs = [fmap (\s -> DL.filter (\c -> c /= '(' && c /= ')') s) $ show <$> (DL.zip sc' sc')]
            ks = DL.concat . DL.transpose $ [DL.replicate (DL.length sc) (T.unpack "-k")] DL.++ scs
            sepChar = BSC.unpack . BS.singleton . renderSeparator $ formatSeparator fmt

          (_, _, _, pr) <- createProcess ( proc "sort"
                                         $ ks <> ["-t", sepChar, "-o", tmp </> "gnu-sorted", tmpFile]
                                         ) { env = Just [("LC_COLLATE", "C")] }
          ex <- waitForProcess pr
          case ex of
            ExitFailure _ -> return $ counterexample "gnu-sort errorred out" False
            ExitSuccess -> do
              (ex', so', _se') <- readProcessWithExitCode "diff" [ "-c"
                                                                   , tmp </> "regiment-sorted"
                                                                   , tmp </> "gnu-sorted"
                                                                 ] ""
              case ex' of
                ExitSuccess -> return $ property True
                ExitFailure _ -> return $ counterexample ("diff failed: " <> so') False

prop_regiment_split_merge =
  gamble genFormat $ \fmt ->
  gamble (listOfN 1 5 $ listOfN 1 100 $ (mkJack_ $ genRow fmt) `suchThat` (not . BS.null)) $ \vrs -> testIO $ do
    -- sort on all columns to avoid having to deal with differences
    -- in ordering of rows when sort keys are the same.
    let sc = SortColumn <$> [0 .. ((formatColumnCount fmt) - 1)]
    withTempDirectory "dist" "regiment-test." $ \tmp -> do
      chunkFiles <- mapM (writeToTmpFile tmp fmt) vrs
      inn <- writeToTmpFile tmp fmt $ concat vrs
      success <- runEitherT $
        regiment (InputFile inn)
                 (Just . OutputFile $ tmp </> "regiment-sorted")
                 sc
                 (formatKind fmt)
                 (formatNewline fmt)
                 (NumColumns (formatColumnCount fmt))
                 (formatSeparator fmt)
                 (MemoryLimit (1024 * 1024))

      case success of
        Left e ->
          return $ counterexample ("regiment errored out: " <> show e) False
        Right _ -> do
          splitSuccess <-
            mapM (\nm -> runEitherT $ do
                   let tmpDir = TempDirectory $ tmp </> "splits" </> (takeBaseName nm)
                   createDirectory tmpDir
                   firstT RegimentIOParseError $
                     split
                      (InputFile nm)
                      tmpDir
                      sc
                      (formatKind fmt)
                      (formatNewline fmt)
                      (NumColumns (formatColumnCount fmt))
                      (formatSeparator fmt)
                      (MemoryLimit (1024 * 1024))) chunkFiles
          case null $ lefts splitSuccess of
            False ->
              return $ counterexample ("split errored out " <> show splitSuccess) False
            True -> do
              mergeSuccess <-
                runEitherT $
                  mergeDirs (fmap (\nm -> TempDirectory $ tmp </> "splits" </> (takeBaseName nm)) chunkFiles)
                            (Just . OutputFile $ tmp </> "regiment-split-merge")
              case mergeSuccess of
                Left e ->
                  return $ counterexample ("merge errored out: " <> show e) False
                Right _ -> do
                  (ex, so, _se) <- readProcessWithExitCode "diff" [ "-c"
                                                                  , tmp </> "regiment-sorted"
                                                                  , tmp </> "regiment-split-merge"
                                                                  ] ""
                  case ex of
                    ExitSuccess -> return $ property True
                    ExitFailure _ -> return $ counterexample ("diff failed: " <> so) False

writeToTmpFile :: FilePath -> Format -> [BS.ByteString] -> IO FilePath
writeToTmpFile tmp fmt vrs = do
    let inp = unlines fmt vrs
    (tmpFile, hFile) <- openTempFile tmp "test-input-"
    BS.hPut hFile inp
    hClose hFile
    return tmpFile

unlines fmt ls =
  let nl = renderNewline . formatNewline $ fmt
  in BS.intercalate nl ls <> nl

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
