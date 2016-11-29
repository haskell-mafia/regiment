{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.IO (
    SortError (..)
  , sort
  , renderSortError
  ) where

import           Data.Text

import           P

import           Regiment.Data

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

data SortError =
    SortError

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile -> OutputDirectory -> SortColumn -> Separator -> MemoryLimit -> EitherT SortError IO ()
sort _inn _out _sc _sep _m = hoistEither . Right $ ()

