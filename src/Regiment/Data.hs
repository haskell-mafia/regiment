{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Data (
    InputFile (..)
  , OutputDirectory (..)
  , SortColumn (..)
  , MemoryLimit (..)
  , NumColumns (..)
  , NumSortKeys (..)
  , Cursor (..)
  , Vanguard (..)
  , TempDirectory (..)
  , Payload (..)
  , SortKey (..)
  , SortKeysWithPayload (..)
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
import qualified Data.Vector.Mutable as MBoxed
import           Data.Word (Word8)

import           P

import           Parsley.Xsv.Data as X

import           System.IO (FilePath)

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
    }
  deriving (Eq, Show)

instance Ord SortKeysWithPayload where
  compare (SortKeysWithPayload sks1 _) (SortKeysWithPayload sks2 _) = compare sks1 sks2

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

--   ┌─────────┬─────────┬─────────┐
--   │  x x x  │  x x x  │  x x x  │
--   │ x x x x │ x x x x │ x x x x │
--   │  x x x  ╔═════════╗  x x x  │
--   │ x x x x ║ cursor^ ║ x x x x │
--   ╘═════════╝         ║  x x x  │
--   │         │         ║ x x x x │
--   │         │         ╚═════════╛ <- vanguard
--   │         │         │         │
--   │         │         │         │
--   └─────────┴─────────┴─────────┘

data Cursor a =
    NonEmpty a SortKeysWithPayload
  | EOF
  deriving (Show)

data Vanguard s a =
  Vanguard {
    vanguard :: MBoxed.MVector s (Cursor a)
  }

instance Eq (Cursor a) where
  (==) x y = compare x y == EQ

instance Ord (Cursor a) where
  compare (NonEmpty _ sksp1) (NonEmpty _ sksp2) = compare sksp1 sksp2
  compare EOF EOF = EQ
  compare EOF _ = GT
  compare _ EOF = LT

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
