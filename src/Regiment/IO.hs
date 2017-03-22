{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    RegimentIOError (..)
  , regiment
  , renderRegimentIOError
  , open
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import           Data.String (String)

import           P

import           Regiment.Data
import           Regiment.Parse
import           Regiment.Vanguard.Base
import           Regiment.Vanguard.IO

import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           System.IO (IO, stdout, IOMode (..), FilePath, Handle, hClose, openBinaryFile)
import           System.IO.Temp (withSystemTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, mapEitherT, runEitherT, firstEitherT)

data RegimentIOError =
    RegimentIOReadKeysFailed
  | RegimentIOReadPastEOF
  | RegimentIONullWrite
  | RegimentIOBytestringParseFailed String
  | RegimentIOUnpackFailed
  | RegimentIOMinOfEmptyVector
  | RegimentIOParseError RegimentParseError
  | RegimentIOMergeError (RegimentMergeError RegimentMergeIOError)
  deriving (Eq, Show)

renderRegimentIOError :: RegimentIOError -> Text
renderRegimentIOError _ =
  "TODO"

regiment :: InputFile
         -> Maybe OutputFile
         -> [SortColumn]
         -> FormatKind
         -> Newline
         -> NumColumns
         -> Separator
         -> MemoryLimit
         -> EitherT RegimentIOError IO ()
regiment inn out sc f n nc sep m = do
  let
    fmt =
      Format {
        formatSeparator = sep
      , formatNewline = n
      , formatColumnCount = numColumns nc
      , formatKind = f
      }
  firstEitherT RegimentIOParseError . newEitherT $
    withSystemTempDirectory "regiment." $ \tmp ->
      runEitherT $ do
        toTempFiles inn (TempDirectory tmp) fmt sc m
        firstT RegimentParseMergeError $ merge (TempDirectory tmp) out

merge :: TempDirectory
      -> Maybe OutputFile
      -> EitherT (RegimentMergeError RegimentMergeIOError) IO ()
merge (TempDirectory tmp) out = mapEitherT R.runResourceT  $ do
  fs <- liftIO $ fmap (filter (flip notElem [".", ".."])) $ getDirectoryContents tmp
  handles <- mapM (open ReadMode) $ fmap (tmp </>) fs
  v <- mapEitherT liftIO $ formVanguardIO handles
  out' <- case out of
            Just (OutputFile o) -> open WriteMode o
            Nothing -> return stdout
  mapEitherT liftIO $ runVanguardIO v out'

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f = do
  snd <$> R.allocate (openBinaryFile f m) hClose

