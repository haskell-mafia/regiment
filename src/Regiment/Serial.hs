{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Serial (
    getKeyedPayload
  , bKeyedPayload
  ) where

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector as Boxed

import           P

import           Regiment.Data

bSizePrefixedBytes :: BS.ByteString -> Builder
bSizePrefixedBytes bs =
  Builder.int32LE (fromIntegral $ BS.length bs) <> Builder.byteString bs

bKeyedPayload :: KeyedPayload -> Builder
bKeyedPayload kp =
  Builder.int32LE (countKeyedPayload kp) <>
  Boxed.foldl (\x bs -> x <> bSizePrefixedBytes bs) mempty (key <$> keys kp) <>
  bSizePrefixedBytes (payload kp)

getSizedPrefixedBytes :: Get ByteString
getSizedPrefixedBytes =
  Get.getByteString =<< fromIntegral <$> Get.getWord32le

getKey :: Get Key
getKey =
  Key <$> getSizedPrefixedBytes

getKeys :: Int -> Get (Boxed.Vector Key)
getKeys n =
  Boxed.replicateM n getKey

getKeyedPayload :: Get KeyedPayload
getKeyedPayload = do
  bcount <- fromIntegral <$> Get.getWord32le -- get blockCount
  KeyedPayload
    <$> getKeys (bcount - 1)
    <*> getSizedPrefixedBytes
