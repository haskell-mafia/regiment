{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , sort
  , renderSortError
  , writeLines
  , readLine
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Boxed

import           Foreign.Storable (Storable(..))
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           Regiment.Data
import           Regiment.Parse (unpack)

import           System.IO (IO)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)

data SortError =
  SortError

data RegimentIOError =
  RegimentIOError

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile -> OutputDirectory -> [SortColumn] -> Separator -> MemoryLimit -> FormatKind -> EitherT SortError IO ()
sort _inn _out _sc _sep _m _f = hoistEither . Right $ ()


blocky :: SortKeysWithPayload -> Block
blocky sksp =
  let
    int32size = (4 :: Int32)
    skBlocks =
      sortKey <$> (sortKeys sksp)
    sizeSKs =
      -- int32size (i.e. size of length of sort-key) + size of sort-key
      Boxed.foldl (\n v -> n + int32size + (fromIntegral $ BS.length v)) 0 skBlocks
    pBlock =
      unPayload $ payload sksp
    sizepayload =
      -- int32size (i.e. size of length of payload) + size of payload
      int32size + (fromIntegral $ BS.length pBlock)
  in
    Block {
        blockCount = fromIntegral $ ((Boxed.length skBlocks) + 1)
      , blockSize = sizeSKs + sizepayload
      , sortKeyBlocks = skBlocks
      , payloadBlock = pBlock
    }

bSizePrefixedBytes :: BS.ByteString -> Builder
bSizePrefixedBytes bs =
  Builder.int32LE (fromIntegral $ BS.length bs) <> Builder.byteString bs

renderSortKeysWithPayload :: Block -> Builder
renderSortKeysWithPayload b =
  if blockCount b == 0 then
    mempty
  else
    Builder.int32LE (blockSize b) <>
    Builder.int32LE (blockCount b) <>
    Boxed.foldl (\x bs -> x <> bSizePrefixedBytes bs) mempty (sortKeyBlocks b) <>
    bSizePrefixedBytes (payloadBlock b)


writeLine :: IO.Handle -> Boxed.Vector BS.ByteString -> EitherT RegimentIOError IO ()
writeLine h v = do
  let
    msksp = unpack v
  case msksp of
    Left _  -> left RegimentIOError
    Right sksp ->
      liftIO $ Builder.hPutBuilder h (renderSortKeysWithPayload $ blocky sksp)

writeLines :: IO.Handle -> Boxed.Vector (Boxed.Vector BS.ByteString) -> EitherT RegimentIOError IO ()
writeLines h vs =
  if Boxed.null vs
  then left RegimentIOError
  else do
    writeLine h (Boxed.head vs)
    writeLines h (Boxed.tail vs)

readLine :: IO.Handle -> IO (Maybe Line)
readLine h = do
  isEOF <- IO.hIsEOF h
  if isEOF
    then return $ Just EOF
    else do
      size <- BS.hGet h 4
      maybeSize <- peekInt32 size
      case maybeSize of
        Nothing -> return Nothing
        Just s -> do
          bl <- BS.hGet h (fromIntegral s)
          let
            maybeSksp = bsToSksp $ Lazy.fromStrict bl
          case maybeSksp of
            Nothing -> return Nothing
            Just sksp -> return . Just $ NonEmpty h sksp


bsToSksp :: Lazy.ByteString -> Maybe SortKeysWithPayload
bsToSksp bs =
  case Get.runGetOrFail getSortKeysWithPayload bs of
    Left _ ->
      Nothing
    Right (_, _, x) ->
      Just x

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

peekInt32 :: ByteString -> IO (Maybe Int32)
peekInt32 (PS fp off len) =
  if len /= 4 then
    pure Nothing
  else
    withForeignPtr fp $ \ptr ->
      Just <$> peekByteOff ptr off
