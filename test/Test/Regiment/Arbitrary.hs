{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Regiment.Arbitrary where

import qualified Data.ByteString as BS
import qualified Data.Vector as Boxed

import           P

import           Regiment.Data

import           System.IO (Handle)

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (Jack, oneof, listOf1, vectorOf, boundedEnum)

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

genCursor :: Int -> Handle -> Jack Cursor
genCursor n h =
  oneof [
      return EOF
    , NonEmpty <$> return h <*> (genSortKeysWithPayload n)
  ]

