{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard.Base (
    RegimentMergeError (..)
  , readCursor
  , formVanguard
  , runVanguard
  , updateMinCursor
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as BS
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed

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

formVanguard :: PrimMonad m
             => (a -> EitherT x m (Maybe KeyedPayload))
             -> [a]
             -> EitherT (RegimentMergeError x) m (Vanguard (PrimState m) a)
formVanguard reader l = do
  v <- Boxed.mapM (readCursor reader) (Boxed.fromList l)
  v' <- Boxed.thaw v
  return $ Vanguard v'

updateMinCursor :: PrimMonad m
                => (a -> EitherT x m (Maybe KeyedPayload))
                -> Vanguard (PrimState m) a
                -> EitherT (RegimentMergeError x) m (Cursor a, Vanguard (PrimState m) a)
updateMinCursor reader v =
  let
    vcs = vanguard v
    len = MBoxed.length vcs
  in
    case len of
      0 -> left $ RegimentMergeVanguardEmptyError
      _ -> do
        when (len > 1) $
          -- linear bubble up of min
          -- TODO: use a heap instead
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
            -> (a -> EitherT x m (Maybe KeyedPayload))
            -> (BS.ByteString -> m ())
            -> EitherT (RegimentMergeError x) m ()
runVanguard v reader writer = do
  (minCursor, v') <- updateMinCursor reader v
  case minCursor of
    EOF -> return ()
    NonEmpty _ kp -> do
      lift . writer $ payload kp
      runVanguard v' reader writer

