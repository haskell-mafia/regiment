{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , sort
  , renderSortError
  , writeLine
  , writeLines
  , readLine
  , getSortKeysWithPayload
  , bSortKeysWithPayload
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

bSizePrefixedBytes :: BS.ByteString -> Builder
bSizePrefixedBytes bs =
  Builder.int32LE (fromIntegral $ BS.length bs) <> Builder.byteString bs

bSortKeysWithPayload :: SortKeysWithPayload -> Builder
bSortKeysWithPayload sksp =
  Builder.int32LE (countSortKeysWithPayload sksp) <>
  Boxed.foldl (\x bs -> x <> bSizePrefixedBytes bs) mempty (sortKey <$> sortKeys sksp) <>
  bSizePrefixedBytes (unPayload $ payload sksp)

writeLine :: IO.Handle -> SortKeysWithPayload -> IO ()
writeLine h sksp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeSortKeysWithPayload $ sksp)
  liftIO $ Builder.hPutBuilder h (bSortKeysWithPayload sksp)

writeLines :: IO.Handle -> Boxed.Vector (Boxed.Vector BS.ByteString) -> EitherT RegimentIOError IO ()
writeLines h vs =
  if Boxed.null vs
    then left RegimentIOError
    else do
      let
        maybeSksp = unpack . Boxed.head $ vs
      case maybeSksp of
        Left _ -> left RegimentIOError
        Right sksp -> liftIO $ writeLine h sksp
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
