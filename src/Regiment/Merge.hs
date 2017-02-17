{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Regiment.Merge (
    merge
  , RegimentResourceError (..)
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource (..))
import qualified Control.Monad.Trans.Resource as R

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as DC
import qualified Data.Conduit.Binary as DCB
import qualified Data.Conduit.List as DCL
import qualified Data.List as DL
import qualified Data.Map as DM
import           Data.String (String)

import           P

import           Regiment.Data

import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           System.IO (IO, FilePath, IOMode(..), Handle, hIsEOF, hGetLine)
import           System.IO (withFile, openBinaryFile, hClose)

import           X.Control.Monad.Trans.Either (mapEitherT, EitherT, hoistEither)

data RegimentResourceError =
    RegimentResourceNoMinFound String
  | RegimentResourceUnexpectedEmpty
  | RegimentResourceUnexpectedEOF
  deriving (Show)

merge :: TempDirectory -> OutputDirectory -> EitherT RegimentResourceError IO ()
merge t o = do
 let
   dir = outputDirectory o
   outputFile = dir </> "sorted"
 mapEitherT R.runResourceT $ do
  mapLinesHandles <- constructMap t


  source mapLinesHandles DC.=$= DCL.mapM (\x -> (liftIO . BSC.putStrLn $ "chunk") >> pure x) DC.$$ DCB.sinkFile outputFile
 liftIO $ BSC.putStrLn "Merge done - " >> BS.readFile outputFile >>= BSC.putStrLn
constructMap :: (MonadResource m, MonadIO m) => TempDirectory -> m (DM.Map Line Handle)
constructMap (TempDirectory t) = do
  filePaths <- liftIO . fmap (filter (flip notElem [".", ".."])) . getDirectoryContents $ t
  handles <- mapM (open ReadMode) filePaths
  lines <- liftIO $ mapM readLine handles
  return . DM.fromList $ DL.zip lines handles

source :: DM.Map Line Handle -> DC.Source (EitherT RegimentResourceError (R.ResourceT IO)) BS.ByteString
source m' = do
  m <- liftIO $ updateMap m'
  when (moreToRead m) $ do
    let
      kvm = minView m
    case kvm of
      Left err ->
        DC.yieldM $ hoistEither $ Left err
      Right ((l, h), m'') -> do
        -- Note m'' is the map with (minKey, handle) removed
        let
          erbs = extractRowFromLineOrBoom l
        case erbs of
          Left err ->
            DC.yieldM . hoistEither $ Left err
          Right bs -> do
            DC.yieldM . hoistEither $ Right bs
        -- recurse but replace minKey with Empty
        source $ DM.insert Empty h m''

extractRowFromLineOrBoom :: Line -> Either RegimentResourceError BS.ByteString
extractRowFromLineOrBoom l =
  case l of
    NonEmpty bs -> Right bs
    Empty -> Left RegimentResourceUnexpectedEmpty
    EOF -> Left RegimentResourceUnexpectedEOF

open :: MonadResource m => IOMode -> FilePath -> m Handle
open m f =
  snd <$> R.allocate (openBinaryFile f m) hClose

moreToRead :: DM.Map Line Handle -> Bool
moreToRead m =
    not (null $ DM.filterWithKey (\k _ -> not (k == EOF)) m)


minView :: DM.Map Line Handle -> Either RegimentResourceError ((Line, Handle), DM.Map Line Handle)
minView m =
  -- TODO: Suspect this is just using Ord on BS.
  -- This won't work for multiple sort keys
  let
    maybekvm = DM.minViewWithKey (DM.filterWithKey (\k _ -> not (k == EOF || k == Empty)) m)
    -- Note: minViewWithKey returns minKey, minValue and the map with this
    -- key, value pair removed
  in
    maybeToRight (RegimentResourceNoMinFound $ DM.showTree m) maybekvm

updateMap :: DM.Map Line Handle -> IO (DM.Map Line Handle)
updateMap m = do
  -- find keys with value Empty
  -- return map as is if none
  -- read from associated handle
  -- and replace Empty entry with EOF or (NonEmpty Line, Handle)
  let
    mh = DM.lookup Empty m
  maybe (pure m) (\h -> updateEmpty h m) mh

updateEmpty :: Handle -> DM.Map Line Handle -> IO (DM.Map Line Handle)
updateEmpty h m = do
  l <- readLine h
  return $ DM.insert l h (DM.delete Empty m)

readLine :: Handle -> IO Line
readLine h = do
  isEOF <- hIsEOF h
  if isEOF
  then return EOF
  else do
    str <- hGetLine h
    return $ NonEmpty (BSC.pack str)
