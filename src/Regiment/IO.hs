{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    RegimentIOError (..)
  , mergeDirs
  , regiment
  , renderRegimentIOError
  , split
  , open
  , createDirectory
  ) where

import           Control.Exception (SomeException)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import qualified Data.Text as T

import           P

import           Regiment.Data
import           Regiment.Parse
import           Regiment.Vanguard.Base
import           Regiment.Vanguard.IO

import qualified System.Directory as SD
import           System.FilePath ((</>))
import           System.IO (IO, stdout, IOMode (..), FilePath, Handle, hClose, openBinaryFile, hFlush)
import           System.IO.Temp (withSystemTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, mapEitherT, left)
import           X.Control.Monad.Trans.Either (runEitherT, firstEitherT, tryEitherT)

data RegimentIOError =
    RegimentIOParseError RegimentParseError
  | RegimentIOMergeError (RegimentMergeError RegimentMergeIOError)
  | RegimentIOCreateDirectoryError TempDirectory Text
  | RegimentIOTmpDirExistsError TempDirectory
  deriving (Eq, Show)

renderRegimentIOError :: RegimentIOError -> Text
renderRegimentIOError err =
  case err of
    RegimentIOParseError e ->
      renderRegimentParseError e
    RegimentIOMergeError e ->
      renderRegimentMergeError renderRegimentMergeIOError e
    RegimentIOCreateDirectoryError (TempDirectory tmp) t ->
         "Failed to create directory "
      <> T.pack tmp
      <> "Error: " <> t
    RegimentIOTmpDirExistsError (TempDirectory tmp) ->
         "Failed to create directory "
      <> T.pack tmp
      <> "\n"
      <> "Target directory should not exist, it will be created and filled."

regiment ::
     InputFile
  -> Maybe OutputFile
  -> [SortColumn]
  -> FormatKind
  -> Newline
  -> NumColumns
  -> Separator
  -> MemoryLimit
  -> EitherT RegimentIOError IO ()
regiment inn out sc f n nc sep m = do
  firstEitherT RegimentIOParseError . newEitherT $
    withSystemTempDirectory "regiment." $ \tmp ->
      R.runResourceT . runEitherT $ do
        mapEitherT liftIO $ split inn (TempDirectory tmp) sc f n nc sep m
        handles <- openDir (TempDirectory tmp)
        mapEitherT liftIO . firstT RegimentParseMergeError $ merge handles out

split ::
     InputFile
  -> TempDirectory
  -> [SortColumn]
  -> FormatKind
  -> Newline
  -> NumColumns
  -> Separator
  -> MemoryLimit
  -> EitherT RegimentParseError IO ()
split inn tmp sc f n nc sep m = do
  let
    fmt =
      Format {
        formatSeparator = sep
      , formatNewline = n
      , formatColumnCount = numColumns nc
      , formatKind = f
      }
  toTempFiles inn tmp fmt sc m

mergeDirs ::
     [TempDirectory]
  -> Maybe OutputFile
  -> EitherT RegimentIOError IO ()
mergeDirs dirs out = firstT RegimentIOMergeError $
  mapEitherT R.runResourceT $ do
    handles <- mapM openDir dirs
    mapEitherT liftIO $ merge (concat handles) out

merge ::
     [Handle]
  -> Maybe OutputFile
  -> EitherT (RegimentMergeError RegimentMergeIOError) IO ()
merge handles out = mapEitherT R.runResourceT $ do
  v <- mapEitherT liftIO $ formVanguardIO handles
  out' <- case out of
            Just (OutputFile o) -> open WriteMode o
            Nothing -> return stdout
  mapEitherT liftIO $ runVanguardIO v out'
  liftIO $ hFlush out'

openDir :: MonadResource m => TempDirectory -> m [Handle]
openDir (TempDirectory tmp) = do
  fs <- liftIO $ fmap (filter (flip notElem [".", ".."])) $ SD.getDirectoryContents tmp
  handles <- mapM (open ReadMode) $ fmap (tmp </>) fs
  return handles

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f = do
  snd <$> R.allocate (openBinaryFile f m) hClose

createDirectory :: (MonadIO m, MonadCatch m) => TempDirectory -> EitherT RegimentIOError m ()
createDirectory t@(TempDirectory tmp) = do
  exists <- liftIO $ SD.doesDirectoryExist tmp
  if exists
    then
      left $ RegimentIOTmpDirExistsError t
    else
      tryEitherT handler . liftIO $ SD.createDirectoryIfMissing True tmp
  where
    handler :: SomeException -> RegimentIOError
    handler e = RegimentIOCreateDirectoryError t $ T.pack (show e)
