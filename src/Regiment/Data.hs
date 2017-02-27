{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Data (
    InputFile (..)
  , OutputDirectory (..)
  , SortColumn (..)
  , MemoryLimit (..)
  , NumColumns (..)
  , NumSortKeys (..)
  , Line (..)
  , TempDirectory (..)
  , Payload (..)
  , SortKey (..)
  , SortKeysWithPayload (..)
  , HandlesLines (..)
  , comma
  , pipe
  , newline
  , sizeSortKeysWithPayload
  , countSortKeysWithPayload
  , module X
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Vector (Vector)
import qualified Data.Vector as Boxed
import           Data.Word (Word8)

import           P

import           Parsley.Xsv.Data as X

import           System.IO (FilePath, Handle)

newtype InputFile =
  InputFile {
    inputFile :: FilePath
  } deriving (Eq, Show)

newtype OutputDirectory =
  OutputDirectory {
    outputDirectory :: FilePath
  } deriving (Eq, Show)

newtype TempDirectory =
  TempDirectory {
    tempDirectory :: FilePath
  } deriving (Eq, Show)

newtype SortColumn =
  SortColumn {
      sortColumn :: Int
    } deriving (Eq, Show, Ord)

newtype MemoryLimit =
  MemoryLimit {
    memoryLimit :: Int
  } deriving (Eq, Show, Ord)

newtype NumColumns =
  NumColumns {
    numColumns :: Int
  } deriving (Eq, Show, Ord)

newtype NumSortKeys =
  NumSortKeys {
    numSortKeys :: Int
  } deriving (Eq, Show, Ord)

newtype SortKey =
  SortKey {
    sortKey :: ByteString
  } deriving (Eq, Show, Ord)

newtype Payload =
  Payload {
    unPayload :: ByteString
  } deriving (Eq, Show)

data SortKeysWithPayload =
  SortKeysWithPayload {
    sortKeys :: Vector SortKey
  , payload :: Payload
  } deriving (Eq, Show)

sizeSortKeysWithPayload :: SortKeysWithPayload -> Int32
sizeSortKeysWithPayload sksp =
  let
    skBlocks =
      sortKey <$> (sortKeys sksp)
    sizeSKs =
      -- int32size (i.e. size of length of sort-key) + size of sort-key
      Boxed.foldl (\n v -> n + int32size + (fromIntegral $ BS.length v)) (0 :: Int32) skBlocks
    pBlock =
      unPayload $ payload sksp
    sizepayload =
      -- int32size (i.e. size of length of payload) + size of payload
      int32size + (fromIntegral $ BS.length pBlock)
  in
    int32size + sizeSKs + sizepayload -- blockCount + sortkeys + payload

countSortKeysWithPayload :: SortKeysWithPayload -> Int32
countSortKeysWithPayload sksp =
  let
    sks = sortKey <$> sortKeys sksp
  in
    fromIntegral $ (1 + Boxed.length sks)
data Line =
    NonEmpty Handle SortKeysWithPayload
  | EOF
  | Empty Handle
  deriving (Eq, Show)

data HandlesLines =
  HandlesLines {
    handlesLines :: Vector Line
  } deriving (Eq, Show)

pipe :: Word8
pipe =
  124 {- '|' -}

newline :: ByteString
newline =
  BS.singleton 10 {- '\n' -}

comma :: Word8
comma =
  44 {- ',' -}

int32size :: Int32
int32size =
  4
