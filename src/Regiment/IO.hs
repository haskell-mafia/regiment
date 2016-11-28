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

import           X.Control.Monad.Trans.Either (EitherT)

data SortError =
    SortError

renderSortError :: SortError -> Text
renderSortError _ =
  "TODO"

sort :: InputFile -> OutputDirectory -> SortColumn -> Separator -> MemoryLimit -> EitherT SortError IO ()
sort inn out sc sep m = undefined

