{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Parse (
    listify
  ) where

import qualified Data.ByteString as BS
-- import           Data.Text

import           P

import           Parsley.Xsv.Parser

import           Regiment.Data

-- import           System.IO (IO, IOMode (..))

-- import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


listify :: InputFile -> FormatKind -> Newline -> Separator -> Int -> [BS.ByteString]
listify _inn _f _n _s _c =
  -- let
  --  format = Format f n s c
  -- in
    []
