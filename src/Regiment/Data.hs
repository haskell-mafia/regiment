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
  , Block (..)
  , HandlesLines (..)
  , comma
  , pipe
  , newline
  , module X
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Vector (Vector)
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

data Block =
  Block {
    blockSize :: !Int32
  , blockCount :: !Int32
  , sortKeyBlocks :: Vector ByteString
  , payloadBlock :: ByteString
  } deriving (Eq, Show)

newtype Payload =
  Payload {
    unPayload :: ByteString
  } deriving (Eq, Show)

data SortKeysWithPayload =
  SortKeysWithPayload {
    sortKeys :: Vector SortKey
  , payload :: Payload
  } deriving (Eq, Show)

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

comma :: Word8
comma =
  44 {- ',' -}

newline :: ByteString
newline =
  B.singleton 10 {- '\n' -}
