{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , RegimentIOError (..)
  , sort
  , renderSortError
  , open
  , bsToKP
  , vecToKP
  ) where

import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
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


bsToKP :: BS.ByteString -> Either RegimentIOError KeyedPayload
bsToKP bs =
  case Get.runGetOrFail getKeyedPayload $ Lazy.fromStrict bs of
    Left (_, _, e) ->
      Left $ RegimentIOBytestringConversionFailed e
    Right (_, _, x) ->
      Right x

vecToKP :: Boxed.Vector BS.ByteString -> Maybe KeyedPayload
vecToKP vbs =
  -- expected format of Vector
  -- [k_1,k_2,...,k_n,payload] where k_i are sortkeys
  -- expect at least one sortkey
  let
    l = Boxed.length vbs
  in
    if l > 1
      then
        let
          p = Boxed.last vbs
          sks = Key <$> Boxed.take (l-1) vbs
        in
          Just $ KeyedPayload sks p
    else
      Nothing

