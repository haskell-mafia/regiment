{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Vanguard.Base (
    RegimentReadError (..)
  , readCursor
  , formVanguard
  , runVanguard
  , updateMinCursor
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed

import           P

import           Regiment.Data

import           X.Control.Monad.Trans.Either (EitherT, left, bimapEitherT)

data RegimentReadError e =
    RegimentReadCursorError e
  | RegimentReadVanguardEmptyError
  deriving (Eq, Show)


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

