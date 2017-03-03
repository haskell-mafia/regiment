{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard.IO (
    RegimentReadError (..)
  , writePayloadToHandle
  , readCursorFromHandle
  , formVanguardFromHandles
  , runVanguardFromHandles
  , updateMinCursorFromHandles
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed

import           Foreign.Storable (Storable(..))
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           Regiment.Data
import           Regiment.IO

import           System.IO (IO, Handle)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, left, bimapEitherT)

data RegimentReadError e =
    RegimentReadCursorError e
  | RegimentReadVanguardEmptyError
  deriving (Eq, Show)

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

formVanguard :: PrimMonad m
             => (a -> EitherT x m (Maybe SortKeysWithPayload))
             -> [a]
             -> EitherT (RegimentReadError x) m (Vanguard (PrimState m) a)
formVanguard reader l = do
  v <- Boxed.mapM (readCursor reader) (Boxed.fromList l)
  v' <- Boxed.thaw v
  return $ Vanguard v'

updateMinCursor :: PrimMonad m
                => (a -> EitherT x m (Maybe SortKeysWithPayload))
                -> Vanguard (PrimState m) a
                -> EitherT (RegimentReadError x) m (Cursor a, Vanguard (PrimState m) a)
updateMinCursor reader v =
  let
    vcs = vanguard v
    len = MBoxed.length vcs
  in
    case len of
      0 -> left $ RegimentReadVanguardEmptyError
      1 -> do
        minCursor <- MBoxed.read vcs 0
        case minCursor of
          EOF -> return (EOF, Vanguard vcs)
          NonEmpty h _ -> do
            nl <- readCursor reader h
            MBoxed.write vcs 0 nl
            return $ (minCursor, Vanguard vcs)
      _ -> do
        for_ [1 .. ((MBoxed.length vcs) - 1)] $ \i -> do
          m <- MBoxed.read vcs 0
          n <- MBoxed.read vcs i
          when (n < m)
              (MBoxed.unsafeSwap vcs 0 i)
        -- elt at index 0 should now be min
        minCursor <-  MBoxed.read vcs 0
        case minCursor of
          EOF -> return (EOF, Vanguard vcs)
          NonEmpty h _ -> do
            nl <- readCursor reader h
            MBoxed.write vcs 0 nl
            return $ (minCursor, Vanguard vcs)

runVanguard :: PrimMonad m
            => Vanguard (PrimState m) a
            -> (a -> EitherT x m (Maybe SortKeysWithPayload))
            -> (Payload -> m ())
            -> EitherT (RegimentReadError x) m ()
runVanguard v reader writer = do
  (minCursor, v') <- updateMinCursor reader v
  case minCursor of
    EOF -> return ()
    NonEmpty _ sksp -> do
      lift . writer $ payload sksp
      runVanguard v' reader writer

writePayloadToHandle :: Handle -> Payload -> IO ()
writePayloadToHandle h p = BS.hPut h (unPayload p)

runVanguardFromHandles :: Vanguard (PrimState IO) Handle -> Handle -> EitherT (RegimentReadError RegimentIOError) IO ()
runVanguardFromHandles v out =
  runVanguard v readSortKeysWithPayloadFromHandle (writePayloadToHandle out)

formVanguardFromHandles :: [Handle]
                        -> EitherT (RegimentReadError RegimentIOError) IO (Vanguard (PrimState IO) Handle)
formVanguardFromHandles handles = do
  formVanguard readSortKeysWithPayloadFromHandle handles


updateMinCursorFromHandles :: Vanguard (PrimState IO) Handle
                           -> EitherT (RegimentReadError RegimentIOError) IO (Cursor Handle, Vanguard (PrimState IO) Handle)
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

