{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , RegimentIOError (..)
  , sort
  , renderSortError
  , open
  ) where

import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import           Data.String (String)
import qualified Data.Vector as Boxed

import           P

import           Regiment.Data
import           Regiment.Serial

import           System.IO (IO, IOMode (..), FilePath, Handle, hClose, openBinaryFile)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

data SortError =
  SortError

data RegimentIOError =
    RegimentIOReadKeysFailed
  | RegimentIOReadPastEOF
  | RegimentIONullWrite
  | RegimentIOBytestringParseFailed String
  | RegimentIOBytestringConversionFailed String
  | RegimentIOUnpackFailed
  | RegimentIOMinOfEmptyVector
  deriving (Eq, Show)

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile
     -> OutputDirectory
     -> [SortColumn]
     -> Separator
     -> MemoryLimit
     -> FormatKind
     -> EitherT SortError IO ()
sort _inn _out _sc _sep _m _f = hoistEither . Right $ ()

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

