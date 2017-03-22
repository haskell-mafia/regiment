{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Data (
    InputFile (..)
  , OutputFile (..)
  , SortColumn (..)
  , MemoryLimit (..)
  , NumColumns (..)
  , Cursor (..)
  , Vanguard (..)
  , TempDirectory (..)
  , Key (..)
  , KeyedPayload (..)
  , comma
  , pipe
  , newline
  , sizeKeyedPayload
  , countKeyedPayload
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

newtype OutputFile =
  OutputFile {
    outputFile :: FilePath
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

newtype Key =
  Key {
    key :: ByteString
  } deriving (Eq, Show, Ord)

data KeyedPayload =
  KeyedPayload {
    keys :: Vector Key
  , payload :: ByteString
  } deriving (Eq, Show)

instance Ord KeyedPayload where
  compare (KeyedPayload kp1 _) (KeyedPayload kp2 _) = compare kp1 kp2

sizeKeyedPayload :: KeyedPayload -> Int32
sizeKeyedPayload kp =
  let
    kBlocks =
      key <$> (keys kp)
    sizeKs =
      -- int32size (i.e. size of length of key) + size of key
      Boxed.foldl (\n v -> n + int32size + (fromIntegral $ BS.length v)) (0 :: Int32) kBlocks
    pBlock =
      payload kp
    sizepayload =
      -- int32size (i.e. size of length of payload) + size of payload
      int32size + (fromIntegral $ BS.length pBlock)
  in
    int32size + sizeKs + sizepayload -- blockCount + keys + payload

countKeyedPayload :: KeyedPayload -> Int32
countKeyedPayload kp =
  let
    ks = key <$> keys kp
  in
    fromIntegral $ (1 + Boxed.length ks)

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
    NonEmpty a KeyedPayload
  | EOF
  deriving (Show)

data Vanguard s a =
  Vanguard {
    vanguard :: MBoxed.MVector s (Cursor a)
  }

instance Eq (Cursor a) where
  (==) x y = compare x y == EQ

instance Ord (Cursor a) where
  compare (NonEmpty _ kp1) (NonEmpty _ kp2) = compare kp1 kp2
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
