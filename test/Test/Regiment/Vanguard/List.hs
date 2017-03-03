{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Regiment.Vanguard.List where

import qualified Data.List as DL

import           Disorder.Jack (Property, arbitrary, gamble)

import           P

import           Regiment.Data
import           Regiment.Vanguard.Base
import           Regiment.Vanguard.List

import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, (===))
import           Test.QuickCheck.Jack (chooseInt, maxSuccess, stdArgs)



prop_runVanguard_unique_sortkeys :: Property
prop_runVanguard_unique_sortkeys =
  gamble (arbitrary `suchThat` (> 0)) $ \numsks ->
  gamble (chooseInt (1, 10)) $ \numLists ->
  gamble (arbitrary `suchThat` (> 0)) $ \maxSizeOfList ->
  gamble (genNestedListOfUniqueSortKeysWithPayload numsks numLists maxSizeOfList) $ \sksps ->
    let
      expected :: [Payload]
      expected = payload <$> (DL.sort $ concat sksps)

      ps = runVanguardFromLists (DL.sort <$> sksps)
    in
      Right expected === (ps :: Either (RegimentReadError ()) [Payload])

prop_runVanguard_possible_dupe_sortkeys :: Property
prop_runVanguard_possible_dupe_sortkeys =
  gamble (arbitrary `suchThat` (> 0)) $ \numsks ->
  gamble (chooseInt (1, 10)) $ \numLists ->
  gamble (genNestedListOfSortKeysWithPayloadIdenticalToSortKeys numsks numLists) $ \sksps ->
    let
      expected = payload <$> (DL.sort $ concat sksps)
      ps = runVanguardFromLists (DL.sort <$> sksps)
    in
      Right expected === (ps :: Either (RegimentReadError ()) [Payload])

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
