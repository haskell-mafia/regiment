{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Data (
    InputFile (..)
  , OutputDirectory (..)
  , Separator (..)
  , SortColumn (..)
  , MemoryLimit (..)
  , pipe
  , newline
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word (Word8)

import           P

import           System.IO (FilePath)

newtype InputFile =
  InputFile {
    inputFile :: FilePath
  } deriving (Eq, Show)

newtype OutputDirectory =
  OutputDirectory {
    outputDirectory :: FilePath
  } deriving (Eq, Show)

newtype Separator =
  Separator {
      separator :: Word8
    } deriving (Eq, Show, Ord)

newtype SortColumn =
  SortColumn {
      sortColumn :: Int
    } deriving (Eq, Show, Ord)

newtype MemoryLimit =
  MemoryLimit {
    memoryLimit :: Int
  } deriving (Eq, Show, Ord)

pipe :: Word8
pipe =
  124 {- '|' -}

newline :: ByteString
newline =
  B.singleton 10 {- '\n' -}