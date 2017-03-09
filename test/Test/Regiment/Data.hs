{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Regiment.Data where

import           Disorder.Core ((=/=))
import           Disorder.Jack (arbitrary)

import           P

import           System.IO (stdin)

import           Test.Regiment.Arbitrary

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack (suchThat, forAllProperties, quickCheckWithResult, forAll, (===))
import           Test.QuickCheck.Jack (Jack, Property, conjoin, property, maxSuccess, stdArgs)


prop_KeyedPayload_ord =
  forAll (arbitrary `suchThat` (> 0)) $ \n ->
    ordLaws (genKP n) compare

prop_Cursor_ord =
  forAll (arbitrary `suchThat` (> 0)) $ \n ->
    ordLaws (genCursor n stdin) compare

ordLaws :: (Eq a, Show a) => Jack a -> (a -> a -> Ordering) -> Property
ordLaws genA f =
  forAll genA $ \c1 ->
  forAll genA $ \c2 ->
  forAll genA $ \c3 ->
    conjoin [
        f c1 c1 === EQ
      , if c1 /= c2 then f c1 c2 =/= EQ else property True
      , f c1 c2 === complimentOrd (f c2 c1)
      , if f c1 c2 /= GT && f c2 c3 /= GT then f c1 c3 =/= GT else property True
      ]

complimentOrd :: Ordering -> Ordering
complimentOrd o =
  case o of
    EQ ->
      EQ
    LT ->
      GT
    GT ->
      LT

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
