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

genBytes :: Jack BS.ByteString
genBytes =
  BS.pack . toList <$> listOf1 boundedEnum

genSortKey :: Jack SortKey
genSortKey =
  SortKey <$> genBytes

genSortKeysWithPayload :: Int -> Jack SortKeysWithPayload
genSortKeysWithPayload numsks =
  SortKeysWithPayload
    <$> Boxed.fromList <$> (vectorOf numsks genSortKey)
    <*> (Payload <$> genBytes)

genSortKeysWithPayloadIdenticalToSortKeys :: Int -> Jack SortKeysWithPayload
genSortKeysWithPayloadIdenticalToSortKeys numsks = do
  bs <- vectorOf numsks genBytes
  sks <- return $ SortKey <$> bs
  p <- return $ BS.concat bs
  return $
    SortKeysWithPayload {
      sortKeys = Boxed.fromList sks
    , payload = Payload p
    }

genNestedListOfSortKeysWithPayloadIdenticalToSortKeys :: Int -> Int -> Jack [[SortKeysWithPayload]]
genNestedListOfSortKeysWithPayloadIdenticalToSortKeys numsks numLists = do
    vectorOf numLists $ listOfN 0 numLists (genSortKeysWithPayloadIdenticalToSortKeys numsks)

genListOfUniqueSortKeysWithPayload :: Int -> Int -> Int -> Jack [SortKeysWithPayload]
genListOfUniqueSortKeysWithPayload prefix numsks maxListLength = do
  sksps <- listOfN 0 maxListLength (genSortKeysWithPayload numsks)
  let
    uniquifier = BSC.pack $ (show prefix) <> "_" <> (show $ length sksps)

    prepend :: BS.ByteString -> SortKeysWithPayload -> SortKeysWithPayload
    prepend bs sksp =
      SortKeysWithPayload {
          sortKeys = (SortKey <$> Boxed.singleton bs) Boxed.++ sortKeys sksp
        , payload = payload sksp }

  return $ prepend uniquifier <$> sksps

genNestedListOfUniqueSortKeysWithPayload :: Int -> Int -> Int -> Jack [[SortKeysWithPayload]]
genNestedListOfUniqueSortKeysWithPayload numsks numLists maxListLength = do
  forM [1 .. numLists] $ \i ->
    genListOfUniqueSortKeysWithPayload i numsks maxListLength

genCursor :: Int -> Handle -> Jack (Cursor Handle)
genCursor n h =
  oneof [
      return EOF
    , NonEmpty <$> return h <*> (genSortKeysWithPayload n)
  ]
