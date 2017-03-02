{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard (
    writeCursor
  , writeChunk
  , readCursor
  , formVanguard
  , updateMinCursor
  , open
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R


import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed

import           Foreign.Storable (Storable(..))
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           Regiment.Data
import           Regiment.IO
import           Regiment.Parse (unpack)

import           System.IO (IO, FilePath, IOMode(..), Handle)
import           System.IO (openBinaryFile, hClose)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, left)

writeCursor :: IO.Handle -> SortKeysWithPayload -> IO ()
writeCursor h sksp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeSortKeysWithPayload $ sksp)
  liftIO $ Builder.hPutBuilder h (bSortKeysWithPayload sksp)

readCursor :: (MonadIO m) => IO.Handle -> EitherT RegimentIOError m Cursor
readCursor h = do
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

writeChunk :: IO.Handle -> Boxed.Vector (Boxed.Vector BS.ByteString) -> EitherT RegimentIOError IO ()
writeChunk h vs =
  if Boxed.null vs
    then left RegimentIONullWrite
    else do
      let
        maybeSksp = unpack . Boxed.head $ vs
      case maybeSksp of
        Left _ -> left RegimentIOUnpackFailed
        Right sksp -> liftIO $ writeCursor h sksp
      writeChunk h (Boxed.tail vs)

formVanguard :: (MonadResource m, MonadIO m)  => [FilePath] -> EitherT RegimentIOError m Vanguard
formVanguard filePaths = do
  handles <- mapM (open ReadMode) filePaths
  v <- Boxed.mapM readCursor (Boxed.fromList handles)
  v' <- liftIO $ Boxed.thaw v
  return $ Vanguard v'

updateMinCursor :: Vanguard -> EitherT RegimentIOError IO (Cursor, Vanguard)
updateMinCursor v =
    let
      vcs = vanguard v
      len = MBoxed.length vcs
    in
      case len of
        0 -> left RegimentIOMinOfEmptyVector
        1 -> MBoxed.read vcs 0 >>= (\m -> return $ (m, v))
        _ -> do
          for_ [1 .. ((MBoxed.length vcs) - 1)] $ \i -> do
            m <- MBoxed.read vcs 0
            n <- MBoxed.read vcs i
            when (n < m)
                (MBoxed.unsafeSwap vcs 0 i)
          -- elt at index 0 should now be min
          minCursor <- MBoxed.read vcs 0
          case minCursor of
            EOF -> return (EOF, Vanguard vcs)
            NonEmpty h _ -> do
              nl <- readCursor h
              MBoxed.write vcs 0 nl
              return $ (minCursor, Vanguard vcs)

peekInt32 :: ByteString -> IO (Maybe Int32)
peekInt32 (PS fp off len) =
  if len /= 4 then
    pure Nothing
  else
    withForeignPtr fp $ \ptr ->
      Just <$> peekByteOff ptr off

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

bsToSksp :: Lazy.ByteString -> Maybe SortKeysWithPayload
bsToSksp bs =
  case Get.runGetOrFail getSortKeysWithPayload bs of
    Left _ ->
      Nothing
    Right (_, _, x) ->
      Just x
