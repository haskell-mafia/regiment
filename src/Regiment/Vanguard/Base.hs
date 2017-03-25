{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard.Base (
    RegimentMergeError (..)
  , readCursor
  , formVanguard
  , runVanguard
  ) where

import           Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as BS
import qualified Data.Heap as DH

import           P

import           Regiment.Data

import           X.Control.Monad.Trans.Either (EitherT, left, bimapEitherT)

data RegimentMergeError e =
    RegimentMergeCursorError e
  | RegimentMergeVanguardEmptyError
  deriving (Eq, Show)

readCursor :: Monad m
           => (a -> EitherT x m (Maybe KeyedPayload))
           -> a
           -> EitherT (RegimentMergeError x) m (Cursor a)
readCursor reader a' = do
  bimapEitherT RegimentMergeCursorError (maybe EOF (NonEmpty a')) (reader a')


formVanguard :: Monad m
             => (a -> EitherT x m (Maybe KeyedPayload))
             -> [a]
             -> EitherT (RegimentMergeError x) m (Vanguard a)
formVanguard reader l = do
  v <- mapM (readCursor reader) l
  return . Vanguard $ DH.fromList v


updateVanguard :: Monad m
               => a
               -> Vanguard a
               -> (a -> EitherT x m (Maybe KeyedPayload))
               -> EitherT (RegimentMergeError x) m (Vanguard a)
updateVanguard h v reader = do
  nextCursor <- readCursor reader h
  return . Vanguard . DH.insert nextCursor $ unVanguard v


runVanguard :: Monad m
            => Vanguard a
            -> (a -> EitherT x m (Maybe KeyedPayload))
            -> (BS.ByteString -> m ())
            -> EitherT (RegimentMergeError x) m ()
runVanguard (Vanguard v) reader writer = do
  when (DH.null v) $
    left $ RegimentMergeVanguardEmptyError
  case DH.minimum v of
    EOF -> return ()
    NonEmpty h kp -> do
      lift . writer $ payload kp
      v' <- updateVanguard h (Vanguard $ DH.deleteMin v) reader
      runVanguard v' reader writer
