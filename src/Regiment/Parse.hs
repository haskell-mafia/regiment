{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Regiment.Parse (
    RegimentParseError (..)
  , renderRegimentParseError
  , toTempFiles
  , selectSortKeys
  , writeCursor
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimState)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.List as L
import qualified Data.Maybe as DM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.Tim as Tim

import           P

import qualified Parsley.Xsv.Parser as Parsley
import qualified Parsley.Xsv.Render as Parsley

import           Regiment.Data
import           Regiment.Serial
import           Regiment.Vanguard.Base
import           Regiment.Vanguard.IO

import           System.IO (IO, IOMode (..))
import qualified System.IO as IO
import           System.FilePath ((</>))

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT, newEitherT, hoistEither)
import qualified X.Data.Vector as Boxed
import qualified X.Data.Vector.Grow as Grow

data RegimentParseError =
    RegimentParseKeyNotFound
  | RegimentParseIONullWrite
  | RegimentParseVectorToKPFailed Text
  | RegimentParseMergeError (RegimentMergeError RegimentMergeIOError)
  deriving (Eq, Show)

renderRegimentParseError :: RegimentParseError -> Text
renderRegimentParseError err =
  case err of
    RegimentParseKeyNotFound ->
      "Regiment Parse Error: sort keys not found in vector of parsed fields"
    RegimentParseIONullWrite ->
      "Regiment Parse Error: write chunk called with a null vector"
    RegimentParseVectorToKPFailed t ->
      "Regiment Parse Error: Vector could not be converted to KeyedPayload. Reason: " <> t
    RegimentParseMergeError e ->
      renderRegimentMergeError renderRegimentMergeIOError e

toTempFiles ::
     InputFile
  -> TempDirectory
  -> Format
  -> [SortColumn]
  -> MemoryLimit
  -> EitherT RegimentParseError IO ()
toTempFiles (InputFile inn) tmpDir f sc (MemoryLimit cap) = do
  let
      p = Parsley.compile f
      innChunkSize = 1024 * 1024 -- TODO: make this configurable?
  newEitherT . IO.withFile inn ReadMode $ \h -> runEitherT $ do
    acc <- Grow.new 1024

    let
      go :: Int -> Int -> Int -> Int -> BS.ByteString -> EitherT RegimentParseError IO ()
      go !counter !drops !partNum !memCounter bytes = do
        case BS.null bytes of
          True -> do
            liftIO (IO.hIsEOF h) >>= \eof -> case eof of
              True -> do
                flushVector acc partNum tmpDir
              False ->
                liftIO (BS.hGetSome h innChunkSize) >>= go counter drops partNum memCounter
          False ->
            Parsley.runRowParser
              p
              bytes
              (Parsley.More $ \moar -> do
                liftIO (BS.hGetSome h innChunkSize) >>= moar)
              (Parsley.Failure $ \rest err ->
                liftIO (T.putStrLn (Parsley.renderRowParseError err))
                  >> go counter (drops + 1) partNum memCounter rest)
              (Parsley.Success $ \rest fields ->
                let
                  sko = selectSortKeys fields f sc
                in
                  case sko of
                    Left _ ->
                      go counter (drops + 1) partNum memCounter rest
                    Right keysWithOriginal -> do
                      let
                        mKeysWithOriginal =
                          Boxed.foldl (\s bs -> s + BS.length bs) 0 keysWithOriginal

                      if memCounter + mKeysWithOriginal > cap
                        then do
                          flushVector acc partNum tmpDir
                          Grow.add acc keysWithOriginal
                          go (counter + 1) drops (partNum + 1) mKeysWithOriginal rest
                        else do
                          Grow.add acc keysWithOriginal
                          go (counter + 1) drops partNum (memCounter + mKeysWithOriginal) rest)

    liftIO (BS.hGetSome h innChunkSize) >>= go (0 :: Int) (0 :: Int) (0 :: Int) (0 :: Int)

selectSortKeys ::
     Parsley.Fields
  -> Format
  -> [SortColumn]
  -> Either RegimentParseError (Boxed.Vector BS.ByteString)
selectSortKeys fields fmt sortColumns =
  let
    parsed = Parsley.getFields fields
    unparsed = Parsley.renderRow fmt parsed <> (Parsley.renderNewline $ formatNewline fmt)
    maybeSortkeys = L.map (\sc -> parsed Boxed.!? (sortColumn sc)) sortColumns
    ks = DM.catMaybes maybeSortkeys
    keyNotFound = and $ L.map isNothing maybeSortkeys
  in
    case keyNotFound of
      True -> Left RegimentParseKeyNotFound
      -- returns a vector consisting of keys and payload
      False -> Right $ (Boxed.fromList ks) Boxed.++ (Boxed.singleton unparsed)

flushVector ::
     Grow.Grow Boxed.MVector (PrimState IO) (Boxed.Vector BS.ByteString)
  -> Int
  -> TempDirectory
  -> EitherT RegimentParseError IO ()
flushVector acc counter (TempDirectory tmp) = do
  mv <- Grow.unsafeElems acc
  Tim.sort mv
  (v :: Boxed.Vector (Boxed.Vector BS.ByteString)) <- Grow.freeze acc
  -- write to TempFile
  newEitherT . IO.withFile (tmp </> (T.unpack $ renderIntegral counter)) WriteMode $ \out -> do
    runEitherT $ writeChunk out v
  -- done using 'v'
  Grow.clear acc

writeChunk ::
     IO.Handle
  -> Boxed.Vector (Boxed.Vector BS.ByteString)
  -> EitherT RegimentParseError IO ()
writeChunk h vs =
  case Boxed.uncons vs of
    Nothing -> left RegimentParseIONullWrite
    Just (bs, tl) -> do
      kp <- hoistEither $ vecToKP bs
      liftIO $ writeCursor h kp
      if Boxed.null tl
        then liftIO $ IO.hFlush h >> return ()
        else writeChunk h tl

writeCursor :: IO.Handle -> KeyedPayload -> IO ()
writeCursor h kp = do
  liftIO $ Builder.hPutBuilder h (Builder.int32LE . sizeKeyedPayload $ kp)
  liftIO $ Builder.hPutBuilder h (bKeyedPayload kp)


vecToKP :: Boxed.Vector BS.ByteString -> Either RegimentParseError KeyedPayload
vecToKP vbs =
  -- expected format of Vector
  -- [k_1,k_2,...,k_n,payload] where k_i are sort keys
  -- expect n >= 1, thus length vbs >= 2
  let
    l = Boxed.length vbs
  in
    case l of
      0 ->
        Left . RegimentParseVectorToKPFailed $ "Empty vector"
      1 ->
        Left . RegimentParseVectorToKPFailed $ "Singleton vector: " <> (T.pack . show $ Boxed.head vbs)
      _ ->
        let
          p = Boxed.last vbs
          sks = Key <$> Boxed.take (l-1) vbs
        in
          Right $ KeyedPayload sks p

