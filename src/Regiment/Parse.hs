{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regiment.Parse (
    toVector
  , unpack
  ) where

import           Control.Monad.Primitive (PrimState)

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Maybe as DM
import qualified Data.Text.IO as T
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.Tim as Tim

import           P

import           Parsley.Xsv.Parser

import           Regiment.Data

import           System.IO (IO, IOMode (..), print)
import qualified System.IO as IO

import qualified X.Data.Vector.Grow as Grow

-- import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

data RegimentParseError = RegimentParseError

toVector ::
     InputFile
  -> FormatKind
  -> Newline
  -> Separator
  -> NumColumns
  -> [SortColumn]
  -> IO ()
toVector (InputFile inn) f n s (NumColumns c) sc  = do
  let
    p = compile $ Format f n s c
  IO.withFile inn ReadMode $ \h -> do
    acc <- Grow.new (10 * 1024)

    let
      go !counter !drops bytes = do
        case BS.null bytes of
          True -> do
            IO.hIsEOF h >>= \eof -> case eof of
              True -> do
                flushVector acc
                IO.putStrLn ("completed processing on: " <> show counter <> " rows with " <> show drops <> " drops." )
              False ->
                BS.hGetSome h (1024 * 1024) >>= go counter drops
          False ->
            runRowParser
              p
              bytes
              (More $ \moar -> do
                flushVector acc
                BS.hGetSome h (1024 * 1024) >>= moar)
              (Failure $ \rest err -> T.putStrLn (renderRowParseError err) >> go counter (drops + 1) rest)
              (Success $ \rest fields ->
                let
                  sko = selectSortKeys (getFields fields) s sc
                in
                  case sko of
                    Left RegimentParseError ->
                      go counter (drops + 1) rest
                    Right keysWithOriginal -> do
                      Grow.add acc keysWithOriginal
                      go (counter + 1) drops rest)

    BS.hGetSome h (1024 * 1024) >>= go (0 :: Int) (0 :: Int)

selectSortKeys ::
     (Boxed.Vector BS.ByteString)
  -> Separator
  -> [SortColumn]
  -> Either RegimentParseError (Boxed.Vector BS.ByteString)
selectSortKeys parsed (Separator s) sortColumns =
  let
    unparsed = BS.intercalate (BS.singleton s) (Boxed.toList parsed)

    maybeSortkeys = L.map (\sc -> parsed Boxed.!? (sortColumn sc)) sortColumns
    sks = DM.catMaybes maybeSortkeys
    keyNotFound = and $ L.map isNothing maybeSortkeys
  in
    case keyNotFound of
      True -> Left RegimentParseError
      -- returns a vector consisting of keys and payload
      False -> Right $ (Boxed.singleton unparsed) Boxed.++ (Boxed.fromList sks)

flushVector ::
      Grow.Grow Boxed.MVector (PrimState IO) (Boxed.Vector BS.ByteString)
   -> IO ()
flushVector acc = do
   mv <- Grow.unsafeElems acc
   Tim.sort mv
   (v :: Boxed.Vector (Boxed.Vector BS.ByteString)) <- Grow.unsafeFreeze acc
   -- write to TempFile
   print v
   -- done using 'v'
   Grow.clear acc

unpack :: Boxed.Vector BS.ByteString -> Either RegimentParseError KeyedPayload
unpack vbs =
  if (Boxed.length vbs > 2)
  then
    let
      p = Boxed.head vbs
      sks = Key <$> Boxed.tail vbs
    in
      Right $ KeyedPayload sks p
  else
    Left RegimentParseError


