{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , RegimentIOError (..)
  , sort
  , renderSortError
  , getSortKeysWithPayload
  , bSortKeysWithPayload
  , open
  ) where

import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Internal (ByteString(..))
import           Data.String (String)
import qualified Data.Vector as Boxed

import           P

import           Regiment.Data

import           System.IO (IO, IOMode (..), FilePath, Handle, hClose, openBinaryFile)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

data SortError =
  SortError

data RegimentIOError =
    RegimentIOReadSortKeysFailed
  | RegimentIOReadPastEOF
  | RegimentIONullWrite
  | RegimentIOBytestringParseFailed String
  | RegimentIOUnpackFailed
  | RegimentIOMinOfEmptyVector
  deriving (Eq, Show)

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile -> OutputDirectory -> [SortColumn] -> Separator -> MemoryLimit -> FormatKind -> EitherT SortError IO ()
sort _inn _out _sc _sep _m _f = hoistEither . Right $ ()

bSizePrefixedBytes :: BS.ByteString -> Builder
bSizePrefixedBytes bs =
  Builder.int32LE (fromIntegral $ BS.length bs) <> Builder.byteString bs

bSortKeysWithPayload :: SortKeysWithPayload -> Builder
bSortKeysWithPayload sksp =
  Builder.int32LE (countSortKeysWithPayload sksp) <>
  Boxed.foldl (\x bs -> x <> bSizePrefixedBytes bs) mempty (sortKey <$> sortKeys sksp) <>
  bSizePrefixedBytes (unPayload $ payload sksp)

getSizedPrefixedBytes :: Get ByteString
getSizedPrefixedBytes =
  Get.getByteString =<< fromIntegral <$> Get.getWord32le

getPayload :: Get Payload
getPayload =
  Payload <$> getSizedPrefixedBytes

getSortKey :: Get SortKey
getSortKey =
  SortKey <$> getSizedPrefixedBytes

getSortKeys :: Int -> Get (Boxed.Vector SortKey)
getSortKeys n =
  Boxed.replicateM n getSortKey

getSortKeysWithPayload :: Get SortKeysWithPayload
getSortKeysWithPayload = do
  bcount <- fromIntegral <$> Get.getWord32le -- get blockCount
  SortKeysWithPayload
    <$> getSortKeys (bcount - 1)
    <*> getPayload

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

