{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard.IO (
    RegimentReadError (..)
  , readCursorIO
  , formVanguardIO
  , readKeyedPayloadIO
  , runVanguardIO
  , updateMinCursorIO
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Primitive (PrimState)

import qualified Data.ByteString as BS
import           Data.ByteString.Internal (ByteString(..))

import           Foreign.Storable (Storable(..))
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           Regiment.Data
import           Regiment.IO
import           Regiment.Vanguard.Base

import           System.IO (IO, Handle)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (EitherT, left)

readKeyedPayloadIO :: MonadIO m
                   => IO.Handle
                   -> EitherT RegimentIOError m (Maybe KeyedPayload)
readKeyedPayloadIO h = do
  isEOF <- liftIO $ IO.hIsEOF h
  if isEOF
    then return Nothing
    else do
      size <- liftIO $ BS.hGet h 4
      maybeSize <- liftIO $ peekInt32 size
      case maybeSize of
        Nothing -> left RegimentIOReadKeysFailed
        Just s -> do
          bs <- liftIO $ BS.hGet h (fromIntegral s)
          case bsToKP bs of
            Left e -> left e
            Right kp -> return $ Just kp

readCursorIO :: MonadIO m
             => IO.Handle
             -> EitherT (RegimentReadError RegimentIOError) m (Cursor Handle)
readCursorIO h =
  readCursor readKeyedPayloadIO h

runVanguardIO :: Vanguard (PrimState IO) Handle
              -> Handle
              -> EitherT (RegimentReadError RegimentIOError) IO ()
runVanguardIO v out =
  runVanguard v readKeyedPayloadIO (BS.hPut out)

formVanguardIO :: [Handle]
               -> EitherT
                  (RegimentReadError RegimentIOError)
                  IO (Vanguard (PrimState IO) Handle)
formVanguardIO handles = do
  formVanguard readKeyedPayloadIO handles

updateMinCursorIO :: Vanguard (PrimState IO) Handle
                  -> EitherT
                     (RegimentReadError RegimentIOError)
                     IO (Cursor Handle, Vanguard (PrimState IO) Handle)
updateMinCursorIO v =
  updateMinCursor readKeyedPayloadIO v

peekInt32 :: ByteString -> IO (Maybe Int32)
peekInt32 (PS fp off len) =
  if len /= 4 then
    pure Nothing
  else
    withForeignPtr fp $ \ptr ->
      Just <$> peekByteOff ptr off

