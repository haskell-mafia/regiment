{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , sort
  , renderSortError
  , writeLine
  , writeChunk
  , readLine
  , constructLines
  , updateMinLine
  , getSortKeysWithPayload
  , bSortKeysWithPayload
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R


import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.String (String)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed

import           Foreign.Storable (Storable(..))
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           Regiment.Data
import           Regiment.Parse (unpack)

import           System.IO (IO, FilePath, IOMode(..), Handle)
import           System.IO (openBinaryFile, hClose)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)

data SortError =
  SortError

data RegimentIOError =
    RegimentIOReadlineFailed
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

writeLine :: IO.Handle -> SortKeysWithPayload -> IO ()
writeLine h sksp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeSortKeysWithPayload $ sksp)
  liftIO $ Builder.hPutBuilder h (bSortKeysWithPayload sksp)

writeChunk :: IO.Handle -> Boxed.Vector (Boxed.Vector BS.ByteString) -> EitherT RegimentIOError IO ()
writeChunk h vs =
  if Boxed.null vs
    then left RegimentIONullWrite
    else do
      let
        maybeSksp = unpack . Boxed.head $ vs
      case maybeSksp of
        Left _ -> left RegimentIOUnpackFailed
        Right sksp -> liftIO $ writeLine h sksp
      writeChunk h (Boxed.tail vs)

readLine :: (MonadIO m) => IO.Handle -> EitherT RegimentIOError m Line
readLine h = do
  isEOF <- liftIO $ IO.hIsEOF h
  if isEOF
    then return EOF
    else do
      size <- liftIO $ BS.hGet h 4
      maybeSize <- liftIO $ peekInt32 size
      case maybeSize of
        Nothing -> left RegimentIOReadlineFailed
        Just s -> do
          bl <- liftIO $ BS.hGet h (fromIntegral s)
          let
            maybeSksp = bsToSksp $ Lazy.fromStrict bl
          case maybeSksp of
            Nothing -> left $ RegimentIOBytestringParseFailed (BSC.unpack bl)
            Just sksp -> return $ NonEmpty h sksp

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

constructLines :: (MonadResource m, MonadIO m)  => [FilePath] -> EitherT RegimentIOError m Lines
constructLines filePaths = do
  handles <- mapM (open ReadMode) filePaths
  v <- Boxed.mapM readLine (Boxed.fromList handles)
  v' <- liftIO $ Boxed.thaw v
  return $ Lines v'

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

updateMinLine :: Lines -> EitherT RegimentIOError IO (Line, Lines)
updateMinLine ls =
    let
      vls = lines ls
      len = MBoxed.length vls
    in
      case len of
        0 -> left RegimentIOMinOfEmptyVector
        1 -> do
          m <- MBoxed.read vls 0
          return $ (m, ls)
        _ -> do
          for_ [1 .. ((MBoxed.length vls) - 1)] $ \i -> do
            m <- MBoxed.read vls 0
            n <- MBoxed.read vls i
            when (n < m)
                (MBoxed.unsafeSwap vls 0 i)
          -- elt at index 0 should now be min
          minLine <- MBoxed.read vls 0
          case minLine of
            EOF -> return (EOF, Lines vls)
            NonEmpty h _ -> do
              nl <- readLine h
              MBoxed.write vls 0 nl
              return $ (minLine, Lines vls)

