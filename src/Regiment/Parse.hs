{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regiment.Parse (
    toTempFiles
  , selectSortKeys
  , RegimentParseError (..)
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimState)

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Maybe as DM
import qualified Data.Text.IO as T
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.Tim as Tim

import           P

import qualified Parsley.Xsv.Parser as Parsley

import           Regiment.Data
import           Regiment.IO

import           System.IO (IO, IOMode (..))
import qualified System.IO as IO
import           System.FilePath ((</>))

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)
import qualified X.Data.Vector.Grow as Grow

data RegimentParseError =
    RegimentParseKeyNotFound
  | RegimentParseIOError RegimentIOError
  deriving (Eq, Show)

toTempFiles ::
     InputFile
  -> TempDirectory
  -> Format
  -> [SortColumn]
  -> Int
  -> EitherT RegimentParseError IO ()
toTempFiles (InputFile inn) tmpDir f sc cap = do
  let
      p = Parsley.compile f
      innChunkSize = 1024 * 1024
  newEitherT . IO.withFile inn ReadMode $ \h -> runEitherT $ do
    acc <- Grow.new cap

    let
      go :: Int -> Int -> Int -> BS.ByteString -> EitherT RegimentParseError IO ()
      go !counter !drops !part bytes = do
        case BS.null bytes of
          True -> do
            liftIO (IO.hIsEOF h) >>= \eof -> case eof of
              True -> do
                flushVector acc part tmpDir
              False ->
                liftIO (BS.hGetSome h innChunkSize) >>= go counter drops part
          False ->
            Parsley.runRowParser
              p
              bytes
              (Parsley.More $ \moar -> do
                liftIO (BS.hGetSome h innChunkSize) >>= moar)
              (Parsley.Failure $ \rest err ->
                liftIO (T.putStrLn (Parsley.renderRowParseError err)) >> go counter (drops + 1) part rest)
              (Parsley.Success $ \rest fields ->
                let
                  parsed = BS.take (BS.length bytes - BS.length rest) bytes
                  sko = selectSortKeys parsed (Parsley.getFields fields) sc
                in
                  case sko of
                    Left _ ->
                      go counter (drops + 1) part rest
                    Right keysWithOriginal -> do
                      l <- Grow.length acc
                      if l + (Boxed.length keysWithOriginal) > cap
                        then do
                          flushVector acc part tmpDir
                          Grow.add acc keysWithOriginal
                          go (counter + 1) drops (part + 1) rest
                        else do
                          Grow.add acc keysWithOriginal
                          go (counter + 1) drops part rest)

    liftIO (BS.hGetSome h innChunkSize) >>= go (0 :: Int) (0 :: Int) (0 :: Int)

selectSortKeys ::
     BS.ByteString
  -> (Boxed.Vector BS.ByteString)
  -> [SortColumn]
  -> Either RegimentParseError (Boxed.Vector BS.ByteString)
selectSortKeys unparsed parsed sortColumns =
  let
    maybeSortkeys = L.map (\sc -> parsed Boxed.!? (sortColumn sc)) sortColumns
    ks = DM.catMaybes maybeSortkeys
    keyNotFound = and $ L.map isNothing maybeSortkeys
  in do
    case keyNotFound of
      True -> Left RegimentParseKeyNotFound
      -- returns a vector consisting of keys and payload
      False -> Right $ (Boxed.fromList ks) Boxed.++ (Boxed.singleton unparsed)

flushVector :: Grow.Grow Boxed.MVector (PrimState IO) (Boxed.Vector BS.ByteString)
            -> Int
            -> TempDirectory
            -> EitherT RegimentParseError IO ()
flushVector acc counter (TempDirectory tmp) = do
  mv <- Grow.unsafeElems acc
  Tim.sort mv
  (v :: Boxed.Vector (Boxed.Vector BS.ByteString)) <- Grow.unsafeFreeze acc
  -- write to TempFile
  newEitherT . IO.withFile (tmp </> (show counter)) WriteMode $ \out -> do
    runEitherT . firstT (\e -> RegimentParseIOError e) $ writeChunk out v
  -- done using 'v'
  Grow.clear acc
