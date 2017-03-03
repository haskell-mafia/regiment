{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard (
    writeCursor
  , writeChunk
  , readCursor
  , readCursorFromHandle
  , formVanguard
  , formVanguardFromHandles
  , updateMinCursor
  , updateMinCursorFromHandles
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)

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

import           System.IO (IO, Handle)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, left, bimapEitherT)

data RegimentReadError e =
    RegimentReadCursorError e
  | RegimentReadVanguardEmptyError
  deriving (Eq, Show)

writeCursor :: IO.Handle -> SortKeysWithPayload -> IO ()
writeCursor h sksp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeSortKeysWithPayload $ sksp)
  liftIO $ Builder.hPutBuilder h (bSortKeysWithPayload sksp)

readSortKeysWithPayloadFromHandle :: MonadIO m
                                  => IO.Handle
                                  -> EitherT RegimentIOError m (Maybe SortKeysWithPayload)
readSortKeysWithPayloadFromHandle h = do
  isEOF <- liftIO $ IO.hIsEOF h
  if isEOF
    then return Nothing
    else do
      size <- liftIO $ BS.hGet h 4
      maybeSize <- liftIO $ peekInt32 size
      case maybeSize of
        Nothing -> left RegimentIOReadSortKeysFailed
        Just s -> do
          bl <- liftIO $ BS.hGet h (fromIntegral s)
          let
            maybeSksp = bsToSksp $ Lazy.fromStrict bl
          case maybeSksp of
            Nothing -> left $ RegimentIOBytestringParseFailed (BSC.unpack bl)
            Just _ -> return $ maybeSksp

readCursorFromHandle :: MonadIO m
                     => IO.Handle
                     -> EitherT (RegimentReadError RegimentIOError) m (Cursor Handle)
readCursorFromHandle h =
  readCursor readSortKeysWithPayloadFromHandle h


readCursor :: Monad m
           => (a -> EitherT x m (Maybe SortKeysWithPayload))
           -> a
           -> EitherT (RegimentReadError x) m (Cursor a)
readCursor reader a' = do
  bimapEitherT RegimentReadCursorError (maybe EOF (NonEmpty a')) (reader a')

formVanguard :: MonadIO m
             => (a -> EitherT x m (Maybe SortKeysWithPayload))
             -> [a]
             -> EitherT (RegimentReadError x) m (Vanguard a)
formVanguard reader l = do
  v <- Boxed.mapM (readCursor reader) (Boxed.fromList l)
  v' <- liftIO $ Boxed.thaw v
  return $ Vanguard v'

updateMinCursor :: (Eq a, MonadIO m)
                => (a -> EitherT x m (Maybe SortKeysWithPayload))
                -> Vanguard a
                -> EitherT (RegimentReadError x) m (Cursor a, Vanguard a)
updateMinCursor reader v =
  let
    vcs = vanguard v
    len = MBoxed.length vcs
  in
    case len of
      0 -> left $ RegimentReadVanguardEmptyError
      1 -> liftIO $ MBoxed.read vcs 0 >>= (\m -> return $ (m, v))
      _ -> do
        for_ [1 .. ((MBoxed.length vcs) - 1)] $ \i -> do
          m <- liftIO $ MBoxed.read vcs 0
          n <- liftIO $ MBoxed.read vcs i
          when (n < m)
              (liftIO $ MBoxed.unsafeSwap vcs 0 i)
        -- elt at index 0 should now be min
        minCursor <- liftIO $ MBoxed.read vcs 0
        case minCursor of
          EOF -> return (EOF, Vanguard vcs)
          NonEmpty h _ -> do
            nl <- readCursor reader h
            liftIO $ MBoxed.write vcs 0 nl
            return $ (minCursor, Vanguard vcs)

formVanguardFromHandles :: MonadIO m
                        => [Handle]
                        -> EitherT (RegimentReadError RegimentIOError) m (Vanguard Handle)
formVanguardFromHandles handles = do
  formVanguard readSortKeysWithPayloadFromHandle handles


updateMinCursorFromHandles :: Vanguard Handle
                           -> EitherT (RegimentReadError RegimentIOError) IO (Cursor Handle, Vanguard Handle)
updateMinCursorFromHandles v =
  updateMinCursor readSortKeysWithPayloadFromHandle v

peekInt32 :: ByteString -> IO (Maybe Int32)
peekInt32 (PS fp off len) =
  if len /= 4 then
    pure Nothing
  else
    withForeignPtr fp $ \ptr ->
      Just <$> peekByteOff ptr off

bsToSksp :: Lazy.ByteString -> Maybe SortKeysWithPayload
bsToSksp bs =
  case Get.runGetOrFail getSortKeysWithPayload bs of
    Left _ ->
      Nothing
    Right (_, _, x) ->
      Just x

writeChunk :: IO.Handle
           -> Boxed.Vector (Boxed.Vector BS.ByteString)
           -> EitherT RegimentIOError IO ()
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
