{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , RegimentIOError (..)
  , sort
  , renderSortError
  , getKeyedPayload
  , bKeyedPayload
  , open
  , writeCursor
  , writeChunk
  ) where

import           Control.Monad.IO.Class (liftIO)
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
import           Regiment.Parse (unpack)

import           System.IO (IO, IOMode (..), FilePath, Handle, hClose, openBinaryFile)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)

data SortError =
  SortError

data RegimentIOError =
    RegimentIOReadKeysFailed
  | RegimentIOReadPastEOF
  | RegimentIONullWrite
  | RegimentIOBytestringParseFailed String
  | RegimentIOUnpackFailed
  | RegimentIOMinOfEmptyVector
  deriving (Eq, Show)

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile
     -> OutputDirectory
     -> [SortColumn]
     -> Separator
     -> MemoryLimit
     -> FormatKind
     -> EitherT SortError IO ()
sort _inn _out _sc _sep _m _f = hoistEither . Right $ ()

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

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

writeCursor :: IO.Handle -> KeyedPayload -> IO ()
writeCursor h kp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeKeyedPayload $ kp)
  liftIO $ Builder.hPutBuilder h (bKeyedPayload kp)

writeChunk :: IO.Handle
           -> Boxed.Vector (Boxed.Vector BS.ByteString)
           -> EitherT RegimentIOError IO ()
writeChunk h vs =
  if Boxed.null vs
    then left RegimentIONullWrite
    else do
      let
        maybeKp = unpack . Boxed.head $ vs
      case maybeKp of
        Left _ -> left RegimentIOUnpackFailed
        Right kp -> liftIO $ writeCursor h kp
      writeChunk h (Boxed.tail vs)
