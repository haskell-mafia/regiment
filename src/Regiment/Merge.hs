{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Merge (
    merge
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as R

import qualified Data.ByteString as BS

import           P

import           Regiment.Data
import           Regiment.Vanguard

import           System.Directory (getDirectoryContents)
import           System.IO (IO, IOMode(..), Handle)

import           X.Control.Monad.Trans.Either (mapEitherT, EitherT)

merge :: TempDirectory
      -> OutputFile
      -> EitherT RegimentIOError IO ()
merge (TempDirectory d) (OutputFile o) = do
  filePaths <- liftIO $ fmap (filter (flip notElem [".", ".."])) $ getDirectoryContents d
  mapEitherT R.runResourceT $ do
    initV <- formVanguard filePaths
    oh <- open WriteMode o
    let
      go :: Vanguard -> Handle -> EitherT RegimentIOError IO ()
      go v h = do
        (minCursor, v') <- updateMinCursor v
        case minCursor of
          EOF -> return ()
          NonEmpty _ sksp -> liftIO $ BS.hPut h (unPayload $ payload sksp)
        go v' h
    mapEitherT liftIO $ go initV oh
