{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Regiment.Arbitrary where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import qualified Data.List as DL
import qualified Data.Vector as Boxed

import           P

import qualified Parsley.Xsv.Render as Parsley

import           Regiment.Data

import           System.IO (Handle)

import           Test.Parsley.Arbitrary

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (Jack, oneof, listOf1, listOfN, vectorOf, boundedEnum)
import           Test.QuickCheck.Jack (chooseInt, arbitrary, suchThat, mkJack_, sublistOf)

genBytes :: Jack BS.ByteString
genBytes =
  BS.pack . toList <$> listOf1 boundedEnum

genKey :: Jack Key
genKey =
  Key <$> genBytes

genKP :: Int -> Jack KeyedPayload
genKP numKeys =
  KeyedPayload
    <$> Boxed.fromList <$> (vectorOf numKeys genKey)
    <*> genBytes

genKPNoPayload :: Int -> Jack KeyedPayload
genKPNoPayload numKeys = do
  bs <- vectorOf numKeys genBytes
  ks <- return $ Key <$> bs
  p <- return $ BS.concat bs
  return $
    KeyedPayload {
      keys = Boxed.fromList ks
    , payload = p
    }

genListKPsNoPayload :: Jack [[KeyedPayload]]
genListKPsNoPayload = do
  numKeys <- arbitrary `suchThat` (> 0)
  numLists <- chooseInt (1,10)
  vectorOf numLists $ listOfN 0 numLists (genKPNoPayload numKeys)

genKPsUniqueKeys :: Int -> Int -> Int -> Jack [KeyedPayload]
genKPsUniqueKeys prefix numKeys maxListLength = do
  kps <- listOfN 0 maxListLength (genKP numKeys)
  let
    uniquifier = BSC.pack $ (show prefix) <> "_" <> (show $ length kps)

    prepend :: BS.ByteString -> KeyedPayload -> KeyedPayload
    prepend bs kp =
      KeyedPayload {
          keys = (Key <$> Boxed.singleton bs) Boxed.++ keys kp
        , payload = payload kp }

  return $ prepend uniquifier <$> kps

genListKPsUniqueKeys :: Jack [[KeyedPayload]]
genListKPsUniqueKeys = do
  numKeys <- arbitrary `suchThat` (> 0)
  numLists <- chooseInt (1,10)
  maxListLength <- arbitrary `suchThat` (> 0)
  forM [1 .. numLists] $ \i ->
    genKPsUniqueKeys i numKeys maxListLength

genCursor :: Int -> Handle -> Jack (Cursor Handle)
genCursor n h =
  oneof [
      return EOF
    , NonEmpty <$> return h <*> (genKP n)
  ]

genListSortColumns :: Format -> Jack [SortColumn]
genListSortColumns fmt = do
  sortcols <- sublistOf [0 .. ((formatColumnCount fmt) - 1)] `suchThat` (not . null)
  return $ SortColumn <$> sortcols

genField :: Format -> Jack BS.ByteString
genField fmt =
  let
    sep = formatSeparator fmt
  in
    mkJack_ $ case formatKind fmt of
      Delimited -> do
        genDelimitedField strBSlistOf1 sep
      Standardized -> do
        genStandardizedField strBSlistOf1 sep

genRealKP :: Format -> [SortColumn] -> Jack KeyedPayload
genRealKP fmt sc = do
  -- gen KeyedPayload using the gens from Parsley so that
  -- we end up with an actual delimited or standardized payload

  -- assume that sc is legit - i.e of length < formatColumnCount and
  -- a subset of 1 .. formatColumnCount
  fields <- vectorOf (formatColumnCount fmt) (genField fmt)
  let
    sortkeys = DL.map (\i -> Key $ fields DL.!! (sortColumn i)) sc
  return $ KeyedPayload {
      keys = Boxed.fromList sortkeys
    , payload = Parsley.renderRow fmt (Boxed.fromList fields) <> (renderNewline $ formatNewline fmt)
  }

strBSlistOf1 :: QC.Gen BS.ByteString
strBSlistOf1 = fmap BSC.pack . QC.listOf1 . QC.elements $
                 ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> "~!@#$%^&*()\n\r\t\""

genNonNullSeparator :: Jack Separator
genNonNullSeparator =
  arbitrary `suchThat` (\sep -> sep /= (Separator . fromIntegral $ ord '\NUL'))


genRestrictedFormat :: Separator -> Jack Format
genRestrictedFormat sep =
  arbitrary `suchThat` (\fmt -> (formatColumnCount fmt) > 0 && (formatKind fmt) == Delimited && (formatSeparator fmt) == sep && (formatNewline fmt) == LF)
