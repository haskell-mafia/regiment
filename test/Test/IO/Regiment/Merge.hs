{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Regiment.Merge where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as DL
import qualified Data.List.Extra as DL

import           Disorder.Core.IO (testIO)
import           Disorder.Corpus

import           P

import           Regiment.Data
import           Regiment.Merge

import           System.IO (IO, FilePath, putStrLn)
import           System.IO.Temp (withTempDirectory)
import           System.FilePath ((</>))

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (forAllProperties, quickCheckWithResult, forAll, (===))
import           Test.QuickCheck.Jack (noShrink, listOf, elements, maxSuccess, stdArgs)

import           X.Control.Monad.Trans.Either (runEitherT)

writeSortedListsToFiles :: FilePath -> [[BS.ByteString]] -> IO ()
writeSortedListsToFiles d l = do
  let
    go :: FilePath -> Int -> [[BS.ByteString]] -> IO ()
    go dir n ls =
      if null ls
      then return ()
      else do
        let
          out = dir </> show n
          contents = BSC.unlines $ DL.sort (DL.head ls)
        putStrLn $ "Writing content to " <> show out
        BSC.putStrLn $ contents

        BS.writeFile out contents
        putStrLn $ "Verify: Reading contents of " <> show out
        BS.readFile out >>= BSC.putStrLn
        go dir (n + 1) (DL.tail ls)

  putStrLn $ "writeSortedLists: Filepath " <> show d
  putStrLn $ show l
  go d 0 l

mergeLists :: [[BS.ByteString]] -> [BS.ByteString]
mergeLists ls' =
  let
    go :: [BS.ByteString] -> [[BS.ByteString]] -> [BS.ByteString]
    go acc ls =
      if null ls
      then acc
      else do
        go (DL.merge acc (DL.sort $ DL.head ls)) (DL.tail ls)
  in
    go [] ls'

mergeFiles :: FilePath -> FilePath -> IO BS.ByteString
mergeFiles t o = do
  res <- runEitherT $ merge (TempDirectory t) (OutputDirectory o)
  case res of
    Right () -> do
      out <- BS.readFile $ o </> "sorted"
      putStrLn $ "Merged file " <> o </> "sorted"
      putStrLn $ "Read merged file: " <> BSC.unpack out
      return out
    Left err -> return (BSC.pack $ show err)

prop_merge_mth =
  forAll (noShrink $ pure [["mth"]]) $ \l ->
    testIO . withTempDirectory "dist" "regiment-test" $ \tmp ->
      withTempDirectory "dist" "regiment-test" $ \tmp2 -> do
        writeSortedListsToFiles tmp l
        res <- mergeFiles tmp tmp2
        let
          expected = BSC.unlines $ mergeLists l
        return $ res === expected

xprop_merge =
  forAll (listOf . listOf $ elements boats) $ \l ->
    testIO . withTempDirectory "dist" "regiment-test" $ \tmp ->
      withTempDirectory "dist" "regiment-test" $ \tmp2 -> do
        writeSortedListsToFiles tmp l
        res <- mergeFiles tmp tmp2
        let
          expected = BSC.unlines $ mergeLists l
        return $ res === expected

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
