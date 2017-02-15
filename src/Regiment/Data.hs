{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Data (
    InputFile (..)
  , OutputDirectory (..)
  , SortColumn (..)
  , MemoryLimit (..)
  , NumColumns (..)
  , Line (..)
  , TempDirectory (..)
  , comma
  , pipe
  , newline
  , module X
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
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

data Line =
    NonEmpty ByteString
  | EOF
  | Empty
  deriving (Eq, Show, Ord)

pipe :: Word8
pipe =
  124 {- '|' -}

comma :: Word8
comma =
  44 {- ',' -}

newline :: ByteString
newline =
  B.singleton 10 {- '\n' -}
